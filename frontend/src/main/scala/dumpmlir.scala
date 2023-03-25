package de.cfaed.sigi

import ast.*
import datamodel.{TypeDescriptor, TypeParm}
import types.*
import types.StackType.canonicalize

import de.cfaed.sigi.eval.Env

import java.io.PrintStream
import scala.io
import scala.annotation.tailrec
import scala.collection.mutable

package dumpmlir {

  import scala.io.Source

  sealed trait MlirValue {

  }

  case class MlirIdent(prefix: String)(id: Int) extends MlirValue {
    override def toString: String = "%" + prefix + id
  }
  case class MlirSymbol(prefix: String)(id: Int) extends MlirValue {
    override def toString: String = "@" + prefix + id
  }

  class IdGenerator[T](maker: Int => T) {
    private var seqNumber = 0
    def cur: T = maker(seqNumber)

    def next(): T = {
      seqNumber += 1
      cur
    }
  }

  sealed trait SigiDialectOp

  class MPopOp(val ty: KStackTypeItem,
               envIdGen: IdGenerator[MlirIdent],
               valIdGen: IdGenerator[MlirIdent]) extends SigiDialectOp {
    val inEnv = envIdGen.cur
    val outEnv = envIdGen.next()

    val outVal = valIdGen.next()
  }

  class MPushOp(val ty: KStackTypeItem,
                envIdGen: IdGenerator[MlirIdent],
                val inVal: MlirIdent) extends SigiDialectOp {
    val inEnv = envIdGen.cur
    val outEnv = envIdGen.next()
  }


  class MlirBuilder(val out: PrintStream) {
    /** Ident of the environment. */
    private val envIdGen = IdGenerator(MlirIdent("env"))
    /** Ident for regular values. */
    private val valIdGen = IdGenerator(MlirIdent(""))
    /** Symbols for quote generation. */
    private val quoteIdGen = IdGenerator(MlirSymbol("quote"))

    private val localSymEnv = mutable.Map[String, MlirIdent]()

    private def mlirType(ty: KStackTypeItem): String = ty match
      case KInt => "!sigi.int"
      case KBool => "!sigi.bool"
      case KString => "!sigi.str"
      case KFun(stack) =>
        val consumed = stack.consumes.map(mlirType).mkString(",")
        val produced = stack.produces.map(mlirType).mkString(",")
        s"!sigi.fun<[$consumed], [$produced]>"
      case KList(item) => s"!sigi.list<${mlirType(item)}>"
      case _ => throw new IllegalArgumentException(s"Cannot emit type with variable $ty")

    private def makeConstAttr[T](ty: KPrimitive[T], value: T): String = ty match
      case KInt => value.toString
      case KBool => value.toString
      case KString => s"\"$value\""

    private var indent = 0

    def println(str: String):Unit = {
      (0 until indent).foreach { _ => out.print("    ") }
      out.println(str)
    }

    def dumpFunction(funDef: TFunDef): Unit = {
      val TFunDef(name, ty, body) = funDef

      println(s" // $name: $ty")
      println(s"func.func @$name(${envIdGen.cur}: !sigi.env) -> !sigi.env {")
      indent += 1
      this.dumpAst(body)
      println(s"return ${envIdGen.cur}: !sigi.env")
      indent -= 1
      println(s"}")
    }

    private def renderOp(op: SigiDialectOp, comment: String = ""): Unit = {
      op match
        case op: MPopOp =>
          println(s"${op.outEnv}, ${op.outVal} = sigi.pop ${op.inEnv}: ${mlirType(op.ty)} $comment")
        case op: MPushOp =>
          println(s"${op.outEnv} = sigi.push ${op.inEnv}, ${op.inVal}: ${mlirType(op.ty)} $comment")
    }

    /**
      * Contract: before this fun is invoked, the [[envIdGen]] is the ID of the last environment.
      * When it exits, the [[envIdGen]] is the ID of another env id.
      *
      * Assumptions: every name is unique in the given term.
      *
      */
    def dumpAst(t: TypedExpr): Unit =
      val envId = envIdGen.cur
      t match
        case TChain(_, a, b) =>
          dumpAst(a)
          dumpAst(b)

        case TPushList(ty, items) =>
          val itemIds = for (item <- items) yield {
            dumpAst(item)
            val pop = new MPopOp(ty.item, envIdGen, valIdGen)
            renderOp(pop)
            pop.outVal
          }
          val args = itemIds.mkString(", ")
          val listId = valIdGen.next()

          val mlirTy = mlirType(ty)

          println(s"$listId = sigi.create_list $args: $mlirTy")
          renderOp(new MPushOp(ty, envIdGen, listId))

        case TPushPrim(ty, value) =>
          val cstId = valIdGen.next()
          val cstAttr = makeConstAttr(ty, value)
          val mlirTy = mlirType(ty)

          println(s"$cstId = sigi.constant $cstAttr: $mlirTy")
          renderOp(new MPushOp(ty, envIdGen, cstId))


        // just pushing one item
        case TFunApply(StackType(Nil, List(ty)), name) =>
          localSymEnv.get(name) match
            case Some(valueId) => renderOp(new MPushOp(ty, envIdGen, valueId))
            case None =>
              val errId = envIdGen.next()
              println(s"$errId = sigi.error $envId, {msg=\"undefined name '$name'\"}")

        // builtin arithmetic
        case TFunApply(StackType(List(a, b), List(c)), name@("+" | "*" | "/" | "%")) =>
          val pop0 = new MPopOp(a, envIdGen, valIdGen)
          val pop1 = new MPopOp(b, envIdGen, valIdGen)

          val result = valIdGen.next()
          val opName = Map("+" -> "add", "-" -> "sub", "/" -> "div", "%" -> "mod")(name)

          renderOp(pop0)
          renderOp(pop1)
          println(s"$result = sigi.$opName ${pop0.outVal}, ${pop1.outVal}: ${mlirType(c)}")
          val pushBack = new MPushOp(c, envIdGen, result)
          renderOp(pushBack)

        // todo general case of function calls.
        //  do we need to monomorphise generic calls?
        case TFunApply(ty, name) => ???
        case TPushQuote(term) =>
          val quoteSym = quoteIdGen.next()
          val cstId = valIdGen.next()

          val ty = KFun(term.stackTy)
          val mlirTy = mlirType(ty)

          println(s"$cstId = sigi.funvalue $quoteSym: $mlirTy")
          renderOp(new MPushOp(ty, envIdGen, cstId))

        case TNameTopN(stackTy, names) =>
          for ((name, ty) <- names.zip(stackTy.consumes)) {
            val pop = new MPopOp(ty, envIdGen, valIdGen)
            localSymEnv.put(name, pop.outVal)
            renderOp(pop, comment = s"// $name")
          }
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


  def dumpAst(out: PrintStream)(t: TypedStmt): Unit = {

    t match
      case fun: TFunDef =>
        new MlirBuilder(out).dumpFunction(fun)
      case TExprStmt(e) =>
        new MlirBuilder(out).dumpAst(e)
      case TBlock(stmts) =>

        for (st <- stmts) dumpAst(out)(st)
  }

  def dumpAst(out: PrintStream)(module: TModule): Unit = {
    for ((_, fun) <- module.functions) {
      new MlirBuilder(out).dumpFunction(fun)
    }

    val mainFun = TFunDef("__main__", module.mainExpr.stackTy, module.mainExpr)
    new MlirBuilder(out).dumpFunction(mainFun)
  }

  def doDumpMlir(out: PrintStream)(in: Source): Unit = {

    val env = Env.Default.toTypingScope
    val res = for {
      parsed <- SigiParser.parseFile(in.mkString)
      typed <- types.typeFile(env)(parsed)
    } yield dumpAst(out)(typed)

    res match
      case Left(err) => println(err)
      case _ =>
  }

  @main
  def dumpMlirMain(): Unit = doDumpMlir(System.out)(io.Source.stdin)
  @main
  def dumpMlirMain2(): Unit = doDumpMlir(System.out)(io.Source.fromString("(1+2)"))


}
