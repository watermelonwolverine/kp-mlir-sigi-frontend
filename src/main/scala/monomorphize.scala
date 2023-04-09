package de.cfaed.sigi

/** Implements the monomorphization logic.
  *
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
package monomorphize {

  import de.cfaed.sigi.ast.*
  import de.cfaed.sigi.given

  import scala.collection.mutable
  import types.{TySubst, *}

  import de.cfaed.sigi.builtins._
  import debug.given


  case class EmittableModule(
    sourceFileName: String,
    functions: Map[FuncId, CompilationStrategy],
    mainExpr: TypedExpr
  ) {
    val mainFun: TFunDef = {
      val mainFuncId = new UserFuncId("__main__", FilePos(mainExpr.pos, sourceFileName))
      TFunDef(mainFuncId, mainExpr.stackTy, mainExpr)
    }
  }


  /**
    * Explore the main expression and instantiate each generic
    * function call.
    *
    * @param module
    * @return
    */
  def monomorphize(module: TModule): Either[SigiCompilationError, EmittableModule] = {


    val ctx = InstantiationContext.newRootCtx(module)
    for {
      newMainExpr <- ctx.rewriteExpr(module.mainExpr)
      () <- ctx.doInstantiate()
    } yield {
      EmittableModule(sourceFileName = module.sourceFileName,
                      ctx.getAllFunctions,
                      newMainExpr)
    }


  }


  private class StackTypeEqualizer(stackType: StackType) {

    private val canon = stackType.simplify

    override def toString: String = s"Eq($canon)"

    override def equals(obj: Any): Boolean = obj match
      case o: StackTypeEqualizer => o.canon.equals(this.canon)
      case _ => false
  }

  object InstantiationContext {
    def newRootCtx(module: TModule)(using debug.TypeInfLogger): InstantiationContext = new InstantiationContext(
      types.makeTySubst({ case a => a }), // empty subst
      module,
      mutable.Map()
      )
  }

  private class InstantiationContext(
    /** Substitute type variables with data types. */
    private val instSubst: TySubst,

    /** Module context. */
    private val module: TModule,

    /** Maps function ids to their currently known instantiations. */
    private val funcInstantiations: mutable.Map[FuncId, TFunDef] = mutable.Map(),
    private val funcInstantiationIds: mutable.Map[FuncId, mutable.Map[StackTypeEqualizer, FuncId]] = mutable.Map(),
    private val nextInstantiationsToDo: mutable.Set[(FuncInstantiationId, EmittableFuncId)] = mutable.Set(),
    private val usedUncompiledBuiltins: mutable.Set[(BuiltinFuncId, CompilationStrategy)] = mutable.Set()
  )(using log: debug.TypeInfLogger) {

    /** All functions required to compile the module. */
    def getAllFunctions: Map[FuncId, CompilationStrategy] =
      (funcInstantiations.values.toList.map(it => it.id -> CompileFunctionDefinition(it)) ++ usedUncompiledBuiltins).toMap

    def groundTy(ty: KStackTypeItem): Either[SigiCompilationError, KDataType] = {
      instSubst.substTy(ty) match
        case dataType: KDataType => Right(dataType)
        case other => Left(new SigiCompilationError(s"Expected ground type: $other"))
    }

    private def groundStackTy(ty: StackType): Either[SigiCompilationError, StackType] = {
      Right(instSubst.substStackType(ty))
    }

    /** Given a callee func id and the actual type at the
      * invocation site, return a function def for the callee
      * func, possibly monomorphized.
      */
    private def resolveGenericCallee(groundTy: StackType, id: EmittableFuncId): Either[SigiCompilationError, FuncId] = {
      val instantiableId = id match
        case BuiltinWithCompilStrategy(_, CompileFunctionDefinition(definition)) => definition.id
        case id@BuiltinWithCompilStrategy(_, compil) =>
          // this is a builtin with an intrinsic implementation, we won't instantiate it
          usedUncompiledBuiltins += (id -> compil)
          return Right(id)
        case other => other

      val byStackType = funcInstantiationIds.getOrElseUpdate(instantiableId, mutable.Map())
      val typeKey = new StackTypeEqualizer(groundTy)
      // The ID is created first to allow recursive calls to refer to the function, even
      // while it being instantiated.
      val instId = byStackType.getOrElseUpdate(typeKey, FuncInstantiationId(instantiableId, groundTy))

      instId match
        case inst: FuncInstantiationId => nextInstantiationsToDo += (inst -> instantiableId)
        case _ =>

      Right(instId)

    }

    def doInstantiate(): Either[SigiCompilationError, Unit] = {
      while (nextInstantiationsToDo.nonEmpty) {
        val work = nextInstantiationsToDo.filterInPlace(tup => !this.funcInstantiations.contains(tup._1)).toList
        nextInstantiationsToDo.clear()
        val done = work.map {
          case (instId, BuiltinWithCompilStrategy(_, CompileFunctionDefinition(definition))) =>
            monomorphizeFunction(instId, instId.instStackTy)(definition)

          case (instId, callee: UserFuncId) =>
            this.module.getFunction(callee).toRight(SigiTypeError.undef(callee.sourceName))
              .flatMap(monomorphizeFunction(instId, instId.instStackTy))

          // shouldn't have ended up in the map
          case (_, callee) => throw new IllegalStateException(s"Cannot instantiate $callee")
        }.flattenList(using compilErrorMerger).map(_ => ())

        if (done.isLeft)
          return done
      }
      Right(())
    }

    private def newSubContext(subst: TySubst) =
      new InstantiationContext(subst,
                               this.module,
                               this.funcInstantiations,
                               this.funcInstantiationIds,
                               this.nextInstantiationsToDo,
                               this.usedUncompiledBuiltins)

    private def monomorphizeFunction(resultId: FuncInstantiationId, groundTy: StackType)(originalFunc: TFunDef): Either[SigiCompilationError, Unit] = {
      for {
        subst <- types.computeInstantiation(groundTy, originalFunc.ty)
        newCtx = newSubContext(subst)
        rewritten <- newCtx.rewriteExpr(originalFunc.body)
      } yield {
        val newDef = TFunDef(
          id = resultId,
          ty = groundTy,
          body = rewritten
          )
        this.funcInstantiations.put(resultId, newDef)
      }
    }

    def rewriteExpr(expr: TypedExpr): Either[SigiCompilationError, TypedExpr] = expr match
      case TChain(stackTy, a, b) =>
        for {
          ra <- rewriteExpr(a)
          rb <- rewriteExpr(b)
        } yield TChain(stackTy, ra, rb) // todo stack type should be reinferred here
      case TEvalBarrier(e) => ???
      case TPushList(ty, items) => ???
      case TPushPrim(ty, value) => Right(expr) // must be ground

      case TFunApply(stackTy, id: EmittableFuncId) =>
        resolveGenericCallee(stackTy, id).map { calleeId =>
          TFunApply(stackTy, calleeId)
        }

      case TFunApply(stackTy, id: StackValueId) =>
        groundStackTy(stackTy).map(st => TFunApply(st, id))

      case TPushQuote(term) =>
        // For quotes, we have to make sure that all captured values are ground.
        // TODO generic quotes will surely be a problem.
        rewriteExpr(term).map(TPushQuote.apply)

      case TNameTopN(stackTy, ids) =>
        groundStackTy(stackTy).map(st => TNameTopN(st, ids))

  }
}
