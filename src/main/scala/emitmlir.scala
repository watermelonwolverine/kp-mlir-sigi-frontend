package de.cfaed.sigi

import ast.*
import datamodel.{TypeDescriptor, TypeParm}
import types.*
import types.StackType.canonicalize

import de.cfaed.sigi.repl.Env

import java.io.PrintStream
import scala.io
import scala.annotation.tailrec
import scala.collection.mutable

package emitmlir {

  import de.cfaed.sigi.builtins.{BuiltinFunSpec, CompileFunctionDefinition, EmitMlirDefinition, FrontendIntrinsic}
  import de.cfaed.sigi.emitmlir.MlirBuilder.{BuiltinIntOps, BuiltinUnaryOps, ClosureT, TargetFunType}

  import scala.collection.immutable.List
  import scala.collection.mutable.ListBuffer
  import scala.io.Source
  import scala.sys.exit
  import debug.{DebugTypeInfLogger, NoopLogger, given}

  sealed trait MlirValue {

  }

  class MlirIdent(prefix: String)(id: String) extends MlirValue {
    override def toString: String = "%" + prefix + id

  }

case class MlirSymbol(name: String) {
  private def escapedName =
    if name.matches("[_a-zA-Z]\\w*")
    then name
    else s"\"$name\""

  override def toString: String = "@" + escapedName
}

  class IdGenerator[T](start: Int, maker: String => T) {
    private var seqNumber: Int = _
    private var curImpl: T = _
    reset()

    def cur: T = curImpl

    def next(suffix: String = ""): T = {
      seqNumber += 1
      curImpl = maker(seqNumber.toString + suffix)
      cur
    }

    def reset(): Unit = {
      seqNumber = start
      curImpl = maker(start.toString)
    }
  }

  // helper type that represents a push operation
  sealed trait SigiDialectOp

  class MPopOp(val ty: String,
               val inEnv: MlirIdent,
               val outEnv: MlirIdent,
               val outVal: MlirIdent) extends SigiDialectOp

  class MPushOp(val ty: String,
                val inEnv: MlirIdent,
                val outEnv: MlirIdent,
                val inVal: MlirIdent) extends SigiDialectOp

  class NameDeduper {
    private val nameHistogram = mutable.Map[String, Int]()
    private val idToName = mutable.Map[FuncId, String]()

    // this is called exactly once per ID
    private def makeName(id: FuncId): String = {
      val count = nameHistogram.updateWith(id.sourceName)(_.map(_ + 1).orElse(Some(1))).get
      if count == 1 then id.sourceName
      else id.sourceName + "$" + count
    }

    def getMlirName(id: FuncId): String =
      id match
        case BuiltinFuncId(name) => s"sigi::$name"
        case id => idToName.getOrElseUpdate(id, makeName(id))
  }

  class MlirBuilder(val out: PrintStream,
                    private var indent: Int = 0,
                    startEnv: Int = 0,
                    startVal: Int = 0) {
    /** Ident of the environment. */
    private val envIdGen = IdGenerator(startEnv, new MlirIdent("s")(_))
    /** Ident for regular values. */
    private val valIdGen = IdGenerator(startVal, new MlirIdent("v")(_))

    private val nameDeduper = new NameDeduper()

    private case class LocalSymDesc(id: StackValueId, mlirName: MlirIdent, mlirType: String)

    /** Map of variable name (Sigi) to mlir ident for those
      * variables that were put on the
      * runtime stack by a [[TNameTopN]] node.
      */
    private val localSymEnv = mutable.Map[StackValueId, LocalSymDesc]()

    private def resetLocals(): Unit = {
      envIdGen.reset()
      valIdGen.reset()
      localSymEnv.clear()
    }

    private def mlirType(ty: KStackTypeItem): String = ty match
      case KInt => "i32"
      case KBool => "i1"
      case KString => "!sigi.str"
      case KFun(_) => ClosureT
      case KList(item) => s"!sigi.list<${mlirType(item)}>"
      // This case is for tvars that were not properly resolved
      // by monomorphization. This is a bug of the frontend, but
      // the better way to report it is to produce invalid code
      // for now.
      case _ => "!sigi.failed_inference"

    private def makeConstAttr[T](ty: KPrimitive[T], value: T): String = ty match
      case KInt => value.toString
      case KBool => if value then "1" else "0"
      case KString => s"\"$value\""

    def println(str: String): Unit = {
      out.print(str.indent(indent * 4))
    }

    private def getSymbol(id: FuncId): MlirSymbol = MlirSymbol(nameDeduper.getMlirName(id))

    def emitMlirImpl(id: FuncId, mlirDefinition: EmitMlirDefinition): Unit = {
      val sym = getSymbol(id)
      println(mlirDefinition.definition(sym.toString).stripIndent())
    }

    def emitFunction(funDef: TFunDef, isMain: Boolean = false): Unit = {
      resetLocals()
      val TFunDef(id, ty, body, _) = funDef

      val funSym = getSymbol(id)
      val visibility = if !id.isInstanceOf[UserFuncId] then " private" else ""

      println("")
      println(s"// ${id.sourceName}: $ty")
      val attrs = if isMain then " attributes {sigi.main}" else ""
      println(s"func.func$visibility $funSym(${envIdGen.cur}: !sigi.stack) -> !sigi.stack$attrs {")
      indent += 1
      this.emitExpr(body)
      println(s"return ${envIdGen.cur}: !sigi.stack")
      indent -= 1
      println(s"}")
    }

    private def renderOp(op: SigiDialectOp, comment: String = ""): Unit = {
      val realComment = if comment.isEmpty then "" else s" // $comment"

      op match
        case op: MPopOp =>
          println(s"${op.outEnv}, ${op.outVal} = sigi.pop ${op.inEnv}: ${op.ty}$realComment")
        case op: MPushOp =>
          println(s"${op.outEnv} = sigi.push ${op.inEnv}, ${op.inVal}: ${op.ty}$realComment")
    }

    private def typeToStr(t: String | KStackTypeItem) = t match
      case s: String => s
      case t: KStackTypeItem => mlirType(t)

    private def renderPush(ty: KStackTypeItem | String,
                           inVal: MlirIdent,
                           inEnv: MlirIdent = envIdGen.cur,
                           comment: String = ""): Unit =
      val push = new MPushOp(ty = typeToStr(ty),
                             inEnv = inEnv,
                             outEnv = envIdGen.next(),
                             inVal = inVal)
      renderOp(push, comment)

    private def makePop(ty: KStackTypeItem | String,
                        valueNameSuffix: String = ""): MPopOp =
      new MPopOp(ty = typeToStr(ty),
                 inEnv = envIdGen.cur,
                 outEnv = envIdGen.next(),
                 outVal = valIdGen.next(valueNameSuffix))


    private def renderPop(ty: KStackTypeItem | String,
                          valueNameSuffix: String = "",
                          comment: String = ""): MlirIdent =
      val pop = makePop(ty, valueNameSuffix)
      renderOp(pop, comment = comment)
      pop.outVal

    /**
      * Contract: before this fun is invoked, the [[envIdGen]] is the ID of the last environment.
      * When it exits, the [[envIdGen]] is the ID of another env id.
      *
      * Assumptions: every name is unique in the given term.
      *
      */
    private def emitExpr(t: TypedExpr): Unit =
      val envId = envIdGen.cur

      def unaryWithConstant(ty: KStackTypeItem, constant: String, arithOp: String, comment: String = ""): Unit =
        val mlirTy = mlirType(ty)
        if (comment.nonEmpty)
          println(s"// $comment")

        val zeroCst = valIdGen.next()
        println(s"$zeroCst = arith.constant $constant: $mlirTy")

        val pop = renderPop(ty)

        val result = valIdGen.next()
        println(s"$result = arith.$arithOp $zeroCst, $pop: $mlirTy")
        renderPush(mlirTy, result)

      t match
        case TChain(_, a, b) =>
          emitExpr(a)
          emitExpr(b)

        case TPushList(ty, items) =>
          val itemIds = for (item <- items) yield {
            emitExpr(item)
            renderPop(ty.item)
          }
          val args = itemIds.mkString(", ")
          val listId = valIdGen.next()

          val mlirTy = mlirType(ty)

          println(s"$listId = sigi.create_list $args: $mlirTy")
          renderPush(ty, listId)

        case TPushPrim(ty, value) =>
          val op = if ty == KBool || ty == KInt then "arith.constant" else "sigi.constant"
          val cstId = valIdGen.next()
          val cstAttr = makeConstAttr(ty, value)
          val mlirTy = mlirType(ty)

          println(s"$cstId = $op $cstAttr: $mlirTy")
          renderPush(ty, cstId)

        // just pushing one item
        case TFunApply(StackType(Nil, List(ty)), binding@StackValueId(name, false, _)) =>
          localSymEnv.get(binding) match
            case Some(LocalSymDesc(_, mlirId, _)) => renderPush(ty, mlirId, comment = s"push $name")
            case None => throw IllegalStateException(s"Unknown stack symbol: $binding")

        // apply a function value captured from the stack
        case TFunApply(ty, binding@StackValueId(name, true, _)) =>
          localSymEnv.get(binding) match
            case Some(LocalSymDesc(_, mlirId, _)) =>
              println(s"${envIdGen.next()} = closure.call $mlirId ($envId) : $ClosureT // call $name: $ty")
            case None => throw IllegalStateException(s"Unknown stack symbol: $binding")

        // builtin binary arithmetic ops and comparisons
        case TFunApply(StackType(List(a@(KInt | KBool), b), List(c)), id: BuiltinFuncId) if a == b && BuiltinIntOps.contains(id) =>
          println(s"// ${id.sourceName}")
          val pop0 = renderPop(b)
          val pop1 = renderPop(a)

          val result = valIdGen.next()
          val builtinOp = BuiltinIntOps(id)

          println(s"$result = $builtinOp $pop0, $pop1: ${mlirType(a)}")
          renderPush(c, result)

        case TFunApply(StackType(List(KInt), List(KInt)), BuiltinFuncId(name@"unary_-")) =>
          unaryWithConstant(KInt, "0", "subi", name)
        // unary_+ is a noop
        case TFunApply(StackType(List(KInt), List(KInt)), BuiltinFuncId("unary_+")) =>
        case TFunApply(StackType(List(KInt), List(KInt)), BuiltinFuncId(name@"unary_~")) =>
          // bitwise not
          unaryWithConstant(KInt, "1", "xori", name)
        case TFunApply(StackType(List(KBool), List(KBool)), BuiltinFuncId(name@"unary_!")) =>
          // bitwise not
          unaryWithConstant(KBool, "1", "xori", name)

        // intrinsics
        case TFunApply(StackType(List(a), _), BuiltinFuncId("dup")) =>
          println(s"// dup intrinsic")
          val popVal = renderPop(a)
          renderPush(a, popVal)
          renderPush(a, popVal)

        case TFunApply(StackType(List(a), _), BuiltinFuncId("pop")) =>
          renderPop(a, comment = "pop intrinsic")

        case TFunApply(_, BuiltinFuncId("pass")) =>
          // this is a noop

        // cond intrinsic
        case TFunApply(StackType(List(KBool, a, b), List(c)), BuiltinFuncId("cond")) if a == b && b == c =>
          val ty = mlirType(c)
          val popElse = renderPop(b)
          val popThen = renderPop(a)
          val popCondition = renderPop(KBool)
          val resultId = valIdGen.next()
          println(
            s"""
               |$resultId = scf.if $popCondition -> $ty {
               |  scf.yield $popThen: $ty
               |} else {
               |  scf.yield $popElse: $ty
               |}""".stripMargin)
          renderPush(ty, resultId)

        // general case of function calls.
        //  todo monomorphise generic calls, make sure all terms are ground
        case TFunApply(ty, id: EmittableFuncId) =>
          // assume the function has been emitted with the given name (this is after monomorphisation)

          val funSym = MlirSymbol(nameDeduper.getMlirName(id))
          val resultEnv = envIdGen.next()
          println(s"$resultEnv = func.call $funSym($envId) : $TargetFunType // $ty")

        case TPushQuote(term) =>

          val freeVars = collectFreeVars(term)
          val capturedVars = localSymEnv.view.filterKeys(freeVars).toList.sortBy(_._1)
          val prevBindings = capturedVars.toMap // save previous bindings
          val capturedArgsMlir = ListBuffer.empty[String]
          val capturedVarsWithNewIds = capturedVars.map { e =>
            val (name, LocalSymDesc(id, mlirId, mlirType)) = e
            // give the variable a fresh name
            val captId = valIdGen.next(s"_${id.sourceName}")
            capturedArgsMlir += s"$captId = $mlirId : $mlirType"
            (name, LocalSymDesc(id, captId, mlirType))
          }.toMap

          // replace those keys
          this.localSymEnv ++= capturedVarsWithNewIds
          val capturedArgsToString = capturedArgsMlir.mkString(", ")

          val cstId = valIdGen.next()
          val closureEnv = envIdGen.next()
          println(s"$cstId = closure.box [$capturedArgsToString] ($closureEnv : !sigi.stack) -> !sigi.stack { // ${term.stackTy}")
          indent += 1
          emitExpr(term)
          println(s"closure.return ${envIdGen.cur}: !sigi.stack")
          indent -= 1
          println(s"}")
          renderPush(MlirBuilder.ClosureT, inVal = cstId, inEnv = envId)

          // restore previous bindings
          this.localSymEnv ++= prevBindings

        case TNameTopN(stackTy, names) =>
          println(s"// ${t.erase}")
          for ((id, ty) <- names.zip(stackTy.consumes)) {
            val popVal = renderPop(ty, valueNameSuffix = s"_${id.sourceName}", comment = id.sourceName + ": " + ty)
            val desc = LocalSymDesc(id = id, mlirName = popVal, mlirType = mlirType(ty))
            localSymEnv.put(id, desc)
          }

        case TEvalBarrier(_) => throw UnsupportedOperationException("Eval barrier is only for the REPL")

    /**
      * Collects all free vars within a term.
      * This is used to determine captured variables in a closure.
      */
    private def collectFreeVars(term: TypedExpr): Set[StackValueId] = {
      case class VarScope(used: Set[StackValueId], bound: Set[StackValueId])

      def collectFreeVarsRec(term: TypedExpr, scope: VarScope): VarScope = {
        term match
          case TChain(_, a, b) =>
            val aScope = collectFreeVarsRec(a, scope)
            collectFreeVarsRec(b, aScope)
          case TFunApply(_, id: StackValueId) => scope.copy(used = scope.used + id)
          case TNameTopN(_, ids) => scope.copy(bound = scope.bound ++ ids)
          case TPushQuote(term) => collectFreeVarsRec(term, scope)
          case TPushList(_, items) => items.foldRight(scope)(collectFreeVarsRec)
          // no other term affects scope
          case _ => scope
      }

      val VarScope(used, bound) = collectFreeVarsRec(term, VarScope(Set.empty, Set.empty))
      used -- bound
    }
  }

  object MlirBuilder {
    private val TargetFunType = "(!sigi.stack) -> !sigi.stack"
    private val ClosureT = s"!closure.box<$TargetFunType>"
    private val BuiltinIntOps = Map(
      "+" -> "arith.addi",
      "-" -> "arith.subi",
      "*" -> "arith.muli",
      "/" -> "arith.divi",
      "%" -> "arith.modi",
      "<=" -> "arith.cmpi \"sle\",",
      ">=" -> "arith.cmpi \"sge\",",
      "<" -> "arith.cmpi \"slt\",",
      ">" -> "arith.cmpi \"sgt\",",

      "=" -> "arith.cmpi \"eq\",",
      "<>" -> "arith.cmpi \"ne\",",

      // boolean
      // todo can be shadowed
      "and" -> "arith.andi",
      "or" -> "arith.ori",
      "xor" -> "arith.xori",
      ).map((a, b) => (BuiltinFuncId(a), b))
    private val BuiltinUnaryOps = Map(
      "unary_-" -> "",
      "unary_~" -> "",
      "unary_!" -> "",
      ).map((a, b) => (BuiltinFuncId(a), b))
  }

  /*
  Overview of generation steps:
  - Each expression is technically a function of type ((!sigi.stack) -> !sigi.stack). Most of them are
   generated inline though. We use the statement as a codegen boundary because it's also a scoping
   boundary.
  - Each function is generated into its own func of type ((!sigi.stack) -> !sigi.stack)
  - Each quote is mapped to a !closure.box<(!sigi.stack) -> !sigi.stack>
    - they may capture their environment. This makes memory safety complicated, in the general
    case we need garbage collection. Let's restrict quotes to only capture closures and integers.
  - Function calls (= variable access):
    - builtins map to builtin operators of the sigi dialect
    - user-defined funs must have a static binding.

  */

  private def emitModule(out: PrintStream)(module: monomorphize.EmittableModule): Unit = {
    out.println("module {")
    val builder = new MlirBuilder(out, indent = 1)

    for ((id, compil) <- module.functions.toBuffer.sortBy(_._1)) {
      compil match
        case CompileFunctionDefinition(fun) => builder.emitFunction(fun)
        case compil@EmitMlirDefinition(_) => builder.emitMlirImpl(id, compil)
        case FrontendIntrinsic => // do nothing, will be handled here
    }

    builder.emitFunction(module.mainFun, isMain = true)

    out.println("}")
  }

  def parseSigiAndEmitMlir(out: PrintStream)(in: Source)(using debug.TypeInfLogger): Option[SigiCompilationError] = {

    val env = Env.Default.toTypingScope
    val res = for {
      parsed <- new SigiParser(in.descr).parseFile(in.mkString)
      module <- types.typeFile(env)(parsed)
      emittable <- monomorphize.monomorphize(module)
    } yield emitModule(out)(emittable)

    res.left.toOption
  }

  @main
  def sigiToMlir(fileName: String): Int =
    val source = fileName match
      case "-" => io.Source.stdin
      case fileName => io.Source.fromFile(fileName)
    parseSigiAndEmitMlir(System.out)(source)(using NoopLogger) match
      case Some(err) =>
        System.err.println("Failed!")
        System.err.println(err)
        1
      case _ => 0
}
