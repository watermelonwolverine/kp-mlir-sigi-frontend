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

package dumpmlir {

  import de.cfaed.sigi.dumpmlir.MlirBuilder.{BuiltinIntOps, TargetFunType}

  import scala.io.Source

  sealed trait MlirValue {

  }

  class MlirIdent(prefix: String)(id: Int) extends MlirValue {
    override def toString: String = "%" + prefix + id

  }
  class MlirSymbol(prefix: String)(id: Int) extends MlirValue, Comparable[MlirSymbol] {
    val simpleName: String = prefix + id
    override def toString: String = "@" + simpleName
    override def compareTo(o: MlirSymbol): Int = simpleName.compareTo(o.simpleName)

  }

  class IdGenerator[T](maker: Int => T) {
    private var seqNumber = 0
    def cur: T = maker(seqNumber)

    def next(): T = {
      seqNumber += 1
      cur
    }

    def reset(): Unit = {
      seqNumber = 0
    }
  }

  // helper type that represents a push operation
  sealed trait SigiDialectOp

  class MPopOp(val ty: KStackTypeItem,
               envIdGen: IdGenerator[MlirIdent],
               valIdGen: IdGenerator[MlirIdent]) extends SigiDialectOp {
    val inEnv = envIdGen.cur
    val outEnv = envIdGen.next()

    val outVal = valIdGen.next()
  }

  class MPushOp(val tyStr: String,
                envIdGen: IdGenerator[MlirIdent],
                val inVal: MlirIdent) extends SigiDialectOp {
    val inEnv = envIdGen.cur
    val outEnv = envIdGen.next()
  }


  class MlirBuilder(val out: PrintStream, private var indent: Int = 0) {
    /** Ident of the environment. */
    private val envIdGen = IdGenerator(new MlirIdent("e")(_))
    /** Ident for regular values. */
    private val valIdGen = IdGenerator(new MlirIdent("")(_))
    /** Symbols for quote generation. */
    private val quoteIdGen = IdGenerator(new MlirSymbol("quote")(_))

    /** Map of quote ID to the body of the quote. */
    private val deferredQuoteGen = mutable.Map[MlirSymbol, TypedExpr]()

    private val localSymEnv = mutable.Map[String, MlirIdent]()

    def resetLocals(): Unit = {
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
      (0 until indent).foreach { _ => out.print("    ") }
      out.println(str)
    }

    def emitFunction(funDef: TFunDef): Unit = {
      resetLocals()
      val TFunDef(name, ty, body) = funDef

      println(s"// $name: $ty")
      println(s"func.func @$name(${envIdGen.cur}: !sigi.env) -> !sigi.env {")
      indent += 1
      this.emitExpr(body)
      println(s"return ${envIdGen.cur}: !sigi.env")
      indent -= 1
      println(s"}")
    }

    def emitQuotes(): Unit = {

      while deferredQuoteGen.nonEmpty do
        // quote generation may create new quotes
        val quotes = deferredQuoteGen.toList.sortBy(_._1)
        deferredQuoteGen.clear()
        for ((id, term) <- quotes) {
          val fun = TFunDef(id.simpleName, term.stackTy, term)
          emitFunction(fun)
        }
    }

    private def renderOp(op: SigiDialectOp, comment: String = ""): Unit = {
      val realComment = if comment.isEmpty then "" else s" // $comment"
      op match
        case op: MPopOp =>
          println(s"${op.outEnv}, ${op.outVal} = sigi.pop ${op.inEnv}: ${mlirType(op.ty)}$realComment")
        case op: MPushOp =>
          println(s"${op.outEnv} = sigi.push ${op.inEnv}, ${op.inVal}: ${op.tyStr}$realComment")
    }

    private def renderPush(ty: KStackTypeItem,
                           envIdGen: IdGenerator[MlirIdent],
                           inVal: MlirIdent) = renderOp(new MPushOp(mlirType(ty), envIdGen, inVal))

    /**
      * Contract: before this fun is invoked, the [[envIdGen]] is the ID of the last environment.
      * When it exits, the [[envIdGen]] is the ID of another env id.
      *
      * Assumptions: every name is unique in the given term.
      *
      */
    def emitExpr(t: TypedExpr): Unit =
      val envId = envIdGen.cur
      t match
        case TChain(_, a, b) =>
          emitExpr(a)
          emitExpr(b)

        case TPushList(ty, items) =>
          val itemIds = for (item <- items) yield {
            emitExpr(item)
            val pop = new MPopOp(ty.item, envIdGen, valIdGen)
            renderOp(pop)
            pop.outVal
          }
          val args = itemIds.mkString(", ")
          val listId = valIdGen.next()

          val mlirTy = mlirType(ty)

          println(s"$listId = sigi.create_list $args: $mlirTy")
          renderPush(ty, envIdGen, listId)

        case TPushPrim(ty, value) if ty == KBool || ty == KInt =>
          val cstId = valIdGen.next()
          val cstAttr = makeConstAttr(ty, value)
          val mlirTy = mlirType(ty)

          println(s"$cstId = arith.constant $cstAttr: $mlirTy")
          renderPush(ty, envIdGen, cstId)

        case TPushPrim(ty, value) =>
          val cstId = valIdGen.next()
          val cstAttr = makeConstAttr(ty, value)
          val mlirTy = mlirType(ty)

          println(s"$cstId = sigi.constant $cstAttr: $mlirTy")
          renderPush(ty, envIdGen, cstId)


        // just pushing one item
        case TFunApply(StackType(Nil, List(ty)), name) =>
          localSymEnv.get(name) match
            case Some(valueId) => renderPush(ty, envIdGen, valueId)
            case None =>
              val errId = envIdGen.next()
              println(s"$errId = sigi.error $envId, {msg=\"undefined name '$name'\"}")

        // builtin arithmetic and comparisons
        case TFunApply(StackType(List(a@(KInt | KBool), b), List(c)), name) if a == b && BuiltinIntOps.contains(name) =>
          val pop0 = new MPopOp(b, envIdGen, valIdGen)
          val pop1 = new MPopOp(a, envIdGen, valIdGen)

          val result = valIdGen.next()
          val builtinOp = BuiltinIntOps(name)

          renderOp(pop0)
          renderOp(pop1)
          println(s"$result = $builtinOp ${pop0.outVal}, ${pop1.outVal}: ${mlirType(a)}")
          renderPush(c, envIdGen, result)

        case TFunApply(StackType(List(a), _), "dup") =>
          val pop = new MPopOp(a, envIdGen, valIdGen)
          renderOp(pop)
          renderPush(a, envIdGen, pop.outVal)
          renderPush(a, envIdGen, pop.outVal)

        case TFunApply(StackType(List(a), _), "pop") =>
          renderOp(new MPopOp(a, envIdGen, valIdGen))

        case TFunApply(StackType(List(a, b), _), "swap") =>
          val popb = new MPopOp(b, envIdGen, valIdGen)
          val popa = new MPopOp(a, envIdGen, valIdGen)
          renderOp(popb)
          renderOp(popa)
          renderPush(b, envIdGen, popb.outVal)
          renderPush(a, envIdGen, popa.outVal)

        // todo general case of function calls.
        //  do we need to monomorphise generic calls?
        case TFunApply(ty, name) =>
          // assume the function has been emitted with the given name (this is after monomorphisation)

          val escapedName = if name.matches("[a-zA-Z]\\w*") then name else s"\"$name\""
          val resultEnv = envIdGen.next()
          println(s"$resultEnv = func.call @$escapedName($envId) : $TargetFunType")

        case TPushQuote(term) =>
          val quoteSym = quoteIdGen.next()
          val cstId = valIdGen.next()
          deferredQuoteGen.put(quoteSym, term)

          val ty = KFun(term.stackTy)

          println(s"$cstId = func.constant $quoteSym: $TargetFunType // $ty")
          renderOp(new MPushOp(TargetFunType, envIdGen, cstId))

        case TNameTopN(stackTy, names) =>
          for ((name, ty) <- names.zip(stackTy.consumes)) {
            val pop = new MPopOp(ty, envIdGen, valIdGen)
            localSymEnv.put(name, pop.outVal)
            renderOp(pop, comment = name)
          }
  }

  object MlirBuilder {
    private val TargetFunType = "!sigi.env -> !sigi.env"
    private val BuiltinIntOps = Map(
      "+" -> "arith.addi",
      "-" -> "arith.subi",
      "/" -> "arith.divi",
      "%" -> "arith.modi",
      "<=" -> "arith.cmpi \"sle\",",
      ">=" -> "arith.cmpi \"sge\",",
      "<" -> "arith.cmpi \"slt\",",
      ">" -> "arith.cmpi \"sgt\",",

      "=" -> "arith.cmpi \"eq\",",
      "<>" -> "arith.cmpi \"ne\",",
    )

  }

  /*
  Overview of generation steps:
  - Each expression is technically a function of type (!sigi.env -> !sigi.env). Most of them are
   generated inline though. We use the statement as a codegen boundary because it's also a scoping
   boundary.
  - Each statement is generated into its own func of type (!sigi.env -> !sigi.env)
  - The body of each quote is generated as its own func of type (!sigi.env -> !sigi.env)
    - Let's say for simplicity's sake that quotes do not capture their environment.
      TODO implement that in the verification rules: quotes cannot refer to names that are not defined
        in the top-level scope.
  - Function calls (= variable access):
    - builtins map to builtin operators of the sigi dialect
    - user-defined funs must have a static binding.

  */

  def emitModule(out: PrintStream)(module: TModule): Unit = {
    out.println("module {")
    val builder = new MlirBuilder(out, indent = 1)
    for ((_, fun) <- module.functions) {
      builder.emitFunction(fun)
    }

    val mainFun = TFunDef("__main__", module.mainExpr.stackTy, module.mainExpr)
    builder.emitFunction(mainFun)

    builder.emitQuotes()


    // todo missing glue code

    out.println("}")
  }

  def parseSigiAndEmitMlir(out: PrintStream)(in: Source): Unit = {

    val env = Env.Default.toTypingScope
    val res = for {
      parsed <- SigiParser.parseFile(in.mkString)
      typed <- types.typeFile(env)(parsed)
    } yield emitModule(out)(typed)

    res match
      case Left(err) => println(err)
      case _ =>
  }

  @main
  def dumpMlirMain(): Unit = parseSigiAndEmitMlir(System.out)(io.Source.stdin)
  @main
  def testDumping(): Unit = parseSigiAndEmitMlir(System.out)(io.Source.fromString("true == false"))


}
