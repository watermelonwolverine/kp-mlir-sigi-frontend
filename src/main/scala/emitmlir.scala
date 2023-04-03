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

  import de.cfaed.sigi.builtins.{BuiltinFunSpec, FrontendIntrinsic, MlirDefinition, StdLibDefinition}
  import de.cfaed.sigi.emitmlir.MlirBuilder.{BuiltinIntOps, BuiltinUnaryOps, TargetFunType}

  import scala.collection.immutable.List
  import scala.collection.mutable.ListBuffer
  import scala.io.Source

  sealed trait MlirValue {

  }

  class MlirIdent(prefix: String)(id: String) extends MlirValue {
    override def toString: String = "%" + prefix + id

  }

  class MlirSymbol(prefix: String)(id: String) extends MlirValue, Comparable[MlirSymbol] {
    val simpleName: String = prefix + id

    override def toString: String = "@" + simpleName

    override def compareTo(o: MlirSymbol): Int = simpleName.compareTo(o.simpleName)

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


  class MlirBuilder(val out: PrintStream,
                    private var indent: Int = 0,
                    startEnv: Int = 0,
                    startVal: Int = 0) {
    /** Ident of the environment. */
    private val envIdGen = IdGenerator(startEnv, new MlirIdent("s")(_))
    /** Ident for regular values. */
    private val valIdGen = IdGenerator(startVal, new MlirIdent("v")(_))

    private case class LocalSymDesc(sourceName: String, mlirName: MlirIdent, mlirType: String)

    /** Map of variable name (Sigi) to mlir ident for those
      * variables that were put on the
      * runtime stack by a [[TNameTopN]] node.
      */
    private val localSymEnv = mutable.Map[String, LocalSymDesc]()

    private def resetLocals(): Unit = {
      envIdGen.reset()
      valIdGen.reset()
      localSymEnv.clear()
    }

    private def mlirType(ty: KStackTypeItem): String = ty match
      case KInt => "i32"
      case KBool => "i1"
      case KString => "!sigi.str"
      case KFun(stack) =>
        val consumed = stack.consumes.map(mlirType).mkString(",")
        val produced = stack.produces.map(mlirType).mkString(",")
        s"!sigi.fun<[$consumed], [$produced]>"
      case KList(item) => s"!sigi.list<${mlirType(item)}>"
      case _ => throw new IllegalArgumentException(s"Cannot emit type with variable $ty")

    private def makeConstAttr[T](ty: KPrimitive[T], value: T): String = ty match
      case KInt => value.toString
      case KBool => if value then "1" else "0"
      case KString => s"\"$value\""

    def println(str: String): Unit = {
      out.print(str.indent(indent * 4))
    }

    def emitFunction(funDef: TFunDef): Unit = {
      resetLocals()
      val TFunDef(name, ty, body) = funDef

      println("")
      println(s"// $name: $ty")
      println(s"func.func @$name(${envIdGen.cur}: !sigi.stack) -> !sigi.stack {")
      indent += 1
      val uniqued = makeNamesUnique(body)
      this.emitExpr(uniqued)
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
        case TFunApply(StackType(Nil, List(ty)), name) =>
          localSymEnv.get(name) match
            case Some(LocalSymDesc(_, mlirId, _)) => renderPush(ty, mlirId, comment = s"push $name")
            case None =>
              val errId = envIdGen.next()
              println(s"$errId = sigi.error $envId, {msg=\"undefined name '$name'\"}")

        // builtin binary arithmetic ops and comparisons
        case TFunApply(StackType(List(a@(KInt | KBool), b), List(c)), name) if a == b && BuiltinIntOps.contains(name) =>
          println(s"// $name")
          val pop0 = renderPop(b)
          val pop1 = renderPop(a)

          val result = valIdGen.next()
          val builtinOp = BuiltinIntOps(name)

          println(s"$result = $builtinOp $pop0, $pop1: ${mlirType(a)}")
          renderPush(c, result)

        case TFunApply(StackType(List(KInt), List(KInt)), name@"unary_-") =>
          unaryWithConstant(KInt, "0", "subi", name)
        // unary_+ is a noop
        case TFunApply(StackType(List(KInt), List(KInt)), "unary_+") =>
        case TFunApply(StackType(List(KInt), List(KInt)), name@"unary_~") =>
          // bitwise not
          unaryWithConstant(KInt, "1", "xori", name)
        case TFunApply(StackType(List(KBool), List(KBool)), name@"unary_!") =>
          // bitwise not
          unaryWithConstant(KBool, "1", "xori", name)

        // intrinsics
        case TFunApply(StackType(List(a), _), "dup") =>
          println(s"// dup intrinsic")
          val popVal = renderPop(a)
          renderPush(a, popVal)
          renderPush(a, popVal)

        case TFunApply(StackType(List(a), _), "pop") =>
          renderPop(a, comment = "pop intrinsic")

        case TFunApply(StackType(List(a, b), _), "swap") =>
          println(s"// swap intrinsic")
          val popb = renderPop(b)
          val popa = renderPop(a)
          renderPush(b, popb)
          renderPush(a, popa)

        // higher-order function.
        case TFunApply(_, "apply") =>
          println(s"// apply intrinsic")
          val pop = makePop(MlirBuilder.ClosureT)
          renderOp(pop)
          val nextEnv = envIdGen.next()
          println(s"$nextEnv = closure.call ${pop.outVal} (${pop.outEnv}) : ${MlirBuilder.ClosureT}")

        // general case of function calls.
        //  todo monomorphise generic calls, make sure all terms are ground
        case TFunApply(ty, name) =>
          // assume the function has been emitted with the given name (this is after monomorphisation)

          val escapedName = if name.matches("[a-zA-Z]\\w*") then name else s"\"$name\""
          val resultEnv = envIdGen.next()
          println(s"$resultEnv = func.call @$escapedName($envId) : $TargetFunType // $ty")

        case TPushQuote(term) =>

          val freeVars = collectFreeVars(term)
          val capturedVars = localSymEnv.view.filterKeys(freeVars)
          val prevBindings = capturedVars.toMap // save previous bindings
          val capturedArgsMlir = ListBuffer.empty[String]
          val capturedVarsWithNewIds = capturedVars.map { e =>
            val (name, LocalSymDesc(sourceName, mlirId, mlirType)) = e
            // give the variable a fresh name
            val captId = valIdGen.next(s"_$sourceName")
            capturedArgsMlir += s"$captId = $mlirId : $mlirType"
            (name, LocalSymDesc(sourceName, captId, mlirType))
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
          for ((name, ty) <- names.zip(stackTy.consumes)) {
            val popVal = renderPop(ty, valueNameSuffix = s"_$name", comment = name)
            val desc = LocalSymDesc(sourceName = name, mlirName = popVal, mlirType = mlirType(ty))
            localSymEnv.put(name, desc)
          }

        case TEvalBarrier(_) => throw UnsupportedOperationException("Eval barrier is only for the REPL")

    /** Makes all names defined within this term unique, so as
      * to remove name shadowing entirely.
      */
    private def makeNamesUnique(term: TypedExpr): TypedExpr = {
      case class DedupVarScope(map: Map[String, String]) extends (String => String) {
        override def apply(name: String): String = map.getOrElse(name, name)

        def makeUnique(names: IterableOnce[String]): DedupVarScope = {
          val newMap = names.iterator.foldLeft(map) { (map, name) =>
            map + (name -> map.get(name).map(_ + "x").getOrElse(name))
          }
          DedupVarScope(newMap)
        }
      }

      def makeNamesUniqueRec(scope: DedupVarScope, term: TypedExpr): (DedupVarScope, TypedExpr) = {
        term match
          case TChain(ty, a, b) =>
            val (aScope, newA) = makeNamesUniqueRec(scope, a)
            val (bScope, newB) = makeNamesUniqueRec(aScope, b)
            (bScope, TChain(ty, newA, newB))
          case TFunApply(ty, name) => (scope, TFunApply(ty, scope(name)))
          case TNameTopN(ty, names) =>
            val dedupedNames = scope.makeUnique(names)
            (dedupedNames, TNameTopN(ty, names.map(dedupedNames)))
          case TPushList(ty, items) =>
            val (lastScope, newItems) = items.foldLeft((scope, List[TypedExpr]())) { (prev, item) =>
              val (scope, newItems) = prev
              val (scope2, newItem) = makeNamesUniqueRec(scope, item)
              (scope2, newItem :: newItems)
            }
            (lastScope, TPushList(ty, newItems))
          case TPushQuote(term) =>
            val (tscope, newTerm) = makeNamesUniqueRec(scope, term)
            (tscope, TPushQuote(newTerm))
          case term => (scope, term)
      }

      makeNamesUniqueRec(DedupVarScope(Map()), term)._2
    }

    /**
      * Collects all free vars within a term.
      * This is used to determine captured variables in a closure.
      */
    private def collectFreeVars(term: TypedExpr): Set[String] = {
      case class VarScope(used: Set[String], bound: Set[String])

      def collectFreeVarsRec(term: TypedExpr, scope: VarScope): VarScope = {
        term match
          case TChain(_, a, b) =>
            val aScope = collectFreeVarsRec(a, scope)
            collectFreeVarsRec(b, aScope)
          case TFunApply(_, name) => scope.copy(used = scope.used + name)
          case TNameTopN(_, names) => scope.copy(bound = scope.bound ++ names)
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
      )
    private val BuiltinUnaryOps = Map(
      "unary_-" -> "",
      "unary_~" -> "",
      "unary_!" -> "",
      )

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

  private case class EmittableModule(
    stdFunctions: Map[String, BuiltinFunSpec],
    userFunctions: Map[String, TFunDef],
    mainExpr: TypedExpr
  )

  private def emitModule(out: PrintStream)(module: EmittableModule): Unit = {
    out.println("module {")
    val builder = new MlirBuilder(out, indent = 1)

    for ((name, fun) <- module.stdFunctions.toBuffer.sortBy(_._1)) {
      fun.compilationStrategy match
        case StdLibDefinition(fun) => // todo need monomorphization // builder.emitFunction(fun)
        case MlirDefinition(definition) => builder.println(definition(name).stripIndent())
        case FrontendIntrinsic => // do nothing, will be handled here
    }

    for ((_, fun) <- module.userFunctions) {
      builder.emitFunction(fun)
    }

    val mainFun = TFunDef("__main__", module.mainExpr.stackTy, module.mainExpr)
    builder.emitFunction(mainFun)

    // todo missing glue code

    out.println("}")
  }

  private def getUsedStdLibFuns(module: TModule): Set[BuiltinFunSpec] = {
    (module.functions.map(_._2.body) ++ List(module.mainExpr))
      .foldLeft(Set[BuiltinFunSpec]()) { (acc, te) =>
        te.reduce(acc) {
          case (TFunApply(_, name), set) =>
            builtins.BuiltinSpecs.get(name).map(set + _).getOrElse(set)
        }
      }
  }

  def parseSigiAndEmitMlir(out: PrintStream)(in: Source): Unit = {

    val env = Env.Default.toTypingScope
    val res = for {
      parsed <- SigiParser.parseFile(in.mkString)
      module <- types.typeFile(env)(parsed)
    } yield {
      val usedStdLibFuns: Map[String, BuiltinFunSpec] = getUsedStdLibFuns(module).map(f => f.surfaceName -> f).toMap
      // todo monomorphize here
      //  recurse through the main expr.
      //  If any called fun is generic, ground it based on the types at the call site.
      //  Collect non-generic and monomorphized funs for emission.
      //  You can't emit a generic fun or a generic quote.

      // todo stack overflow when typing generic quotes:  1 2 true if { -> x, y; x } else {-> x, y ; y} apply
      emitModule(out)(EmittableModule(usedStdLibFuns, module.functions, module.mainExpr))
    }

    res match
      case Left(err) => println(err)
      case _ =>
  }

  @main
  def sigiToMlir(): Unit = parseSigiAndEmitMlir(System.out)(io.Source.stdin)
  @main
  def testDumping(): Unit = parseSigiAndEmitMlir(System.out)(io.Source.fromString("1 -> x; { x } 2 -> x; { x }"))


}
