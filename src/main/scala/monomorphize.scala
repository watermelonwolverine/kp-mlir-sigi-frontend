package de.cfaed.sigi

/** Implements the monomorphization logic.
  *
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
package monomorphize {

  import de.cfaed.sigi.ast.*
  import de.cfaed.sigi.given

  import scala.collection.mutable
  import types.*

  import de.cfaed.sigi.builtins.*
  import debug.given

  import de.cfaed.sigi.repl.Env


  case class EmittableModule(
    sourceFileName: String,
    functions: Map[FuncId, CompilationStrategy],
    mainExpr: TypedExpr
  ) {
    val mainFun: TFunDef = {
      val mainFuncId = new UserFuncId("__main__", FilePos(mainExpr.pos, sourceFileName))
      val scope = Env.Default.toTypingScope.addBindings(
        functions.collect { case (id: UserFuncId, CompileFunctionDefinition(definition)) => definition.toVarBinding }
        )
      TFunDef(mainFuncId, mainExpr.stackTy, mainExpr, scope.withContext)
    }
  }


  /**
    * Explore the main expression and instantiate each generic
    * function call.
    *
    * @param module
    * @return
    */
  def monomorphize(module: TModule)(using debug.TypeInfLogger): Either[SigiCompilationError, EmittableModule] = {


    val ctx = new InstantiationContext(module)
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
    def newRootCtx(module: TModule)(using debug.TypeInfLogger): InstantiationContext = new InstantiationContext(module)
  }

  private class InstantiationContext(
    /** Module context. */
    private val module: TModule,
  )(using log: debug.TypeInfLogger) {

    private val typingCtx: TypingCtx = TypingCtx()

    /** Maps function ids to their currently known instantiations. */
    private val funcInstantiations: mutable.Map[FuncId, TFunDef] = mutable.LinkedHashMap()
    private val funcInstantiationIds: mutable.Map[FuncId, mutable.Map[StackTypeEqualizer, FuncId]] = mutable.Map()
    private val nextInstantiationsToDo: mutable.Set[(FuncInstantiationId, EmittableFuncId)] = mutable.Set()
    private val usedUncompiledBuiltins: mutable.Set[(BuiltinFuncId, CompilationStrategy)] = mutable.LinkedHashSet()

    /** All functions required to compile the module. */
    def getAllFunctions: Map[FuncId, CompilationStrategy] =
      (funcInstantiations.values.toList.map(it => it.id -> CompileFunctionDefinition(it)) ++ usedUncompiledBuiltins).toMap

    /** Given a callee func id and the actual type at the
      * invocation site, return a function id for the callee
      * func, possibly monomorphized. The actual job of generating
      * code for the func def is deferred until [[doInstantiate]].
      * Equal parameterizations are reused.
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

    private def monomorphizeFunction(resultId: FuncInstantiationId, groundTy: StackType)(originalFunc: TFunDef): Either[SigiCompilationError, Unit] = {
      for {
        retyped <- retypeFunctionBody(originalFunc.scope(typingCtx), groundTy, originalFunc.body)
        rewritten <- rewriteExpr(retyped)
      } yield {
        val newDef = TFunDef(
          id = resultId,
          ty = groundTy,
          body = rewritten,
          scope = originalFunc.scope
          )
        this.funcInstantiations.put(resultId, newDef)
      }
    }


    /**
      * Retype the body of a function with a given target type.
      * This gives a more specific type to all sub-terms based
      * on type information known at the call site. If it is
      * done in call graph order, then it should propagate
      * type information naturally and eventually all terms should be ground.
      */
    private def retypeFunctionBody(targetType: StackType, originalFunc: TFunDef): Either[SigiCompilationError, TypedExpr] =
      val scope = originalFunc.scope(this.typingCtx)
      types.assignType(scope)(Some(targetType))(originalFunc.body.erase)


    /** Rewrite function calls in the expr to bind to monomorphized function IDs.
      * This side effects on this object and records the functions to instantiate.
      */
    def rewriteExpr(expr: TypedExpr): Either[SigiCompilationError, TypedExpr] = expr match
      case TChain(stackTy, a, b) =>
        for {
          ra <- rewriteExpr(a)
          rb <- rewriteExpr(b)
        } yield TChain(stackTy, ra, rb)
      case TPushList(ty, items) => items.map(rewriteExpr).flattenList.map(items => TPushList(ty, items))
      case TFunApply(stackTy, id: EmittableFuncId) =>
        resolveGenericCallee(stackTy, id).map { calleeId =>
          TFunApply(stackTy, calleeId)
        }
      case TPushQuote(term) =>
        // For quotes, we have to make sure that all captured values are ground.
        // TODO generic quotes will surely be a problem.
        rewriteExpr(term).map(TPushQuote.apply)
      case _ => Right(expr)

  }
}
