package de.cfaed.kitten

import ast.*
import datamodel.{TypeDescriptor, TypeParm}
import types.*
import types.StackType.canonicalize

import de.cfaed.kitten.eval.Env

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

    def dumpFunction(funDef: TFunDef): Unit = {
      val TFunDef(name, ty, body) = funDef

      out.println(s" // $name: $ty")
      out.println(s"fun @$name(${envIdGen.cur}: !sigi.env) -> !sigi.env {")
      this.dumpAst(body)
      out.println(s"}")
    }


    /**
      * Contract: before this fun is invoked, the [[envIdGen]] is the ID of the last environment.
      * When it exits, the [[envIdGen]] is the ID of another env id.
      *
      * Assumptions: every name is unique in the given term.
      *
      */
    def dumpAst(t: TypedExpr): Unit = t match
      case TChain(_, a, b) =>
        dumpAst(a)
        dumpAst(b)

      case TPushList(ty, items) =>
        val envId = envIdGen.cur
        val itemIds = for (item <- items) yield {
          dumpAst(item)
          envIdGen.cur
        }
        val args = itemIds.mkString(", ")
        val listId = valIdGen.next()
        val pushId = envIdGen.next()

        val mlirTy = mlirType(ty)

        out.println(s"$listId = sigi.create_list $args: $mlirTy")
        out.println(s"$pushId = sigi.push $envId, $listId: $mlirTy")

      case TPushPrim(ty, value) =>
        val envId = envIdGen.cur
        val cstId = valIdGen.next()
        val pushId = envIdGen.next()

        val cstAttr = makeConstAttr(ty, value)
        val mlirTy = mlirType(ty)

        out.println(s" $cstId = sigi.constant $cstAttr: $mlirTy")
        out.println(s" $pushId = sigi.push $envId $cstId: $mlirTy")


      case TFunApply(stackTy, name) => stackTy match
        // todo builtins
        // todo make sure stack type is ground
        case StackType(Nil, List(ty)) =>
          val envId = envIdGen.cur
          localSymEnv.get(name) match
            case Some(valueId) =>
              val pushId = envIdGen.next()
              val mlirTy = mlirType(ty)
              out.println(s"$pushId = sigi.push $envId, $valueId: $mlirTy")
            case None =>
              val errId = envIdGen.next()
              out.println(s"$errId = sigi.error $envId, {msg=\"undefined name '$name'\"}")
        case _ => ??? // actual function call

      case TPushQuote(term) =>
        val envId = envIdGen.cur
        val quoteSym = quoteIdGen.next()
        val cstId = valIdGen.next()
        val pushId = envIdGen.next()

        val mlirTy = mlirType(KFun(term.stackTy))

        out.println(s"$cstId = sigi.funvalue $quoteSym: $mlirTy")
        out.println(s"$pushId = sigi.push $envId, $cstId: $mlirTy")

      case TNameTopN(stackTy, names) =>
        var envId = envIdGen.cur

        for ((name, ty) <- names.zip(stackTy.consumes)) {
          val popId = envIdGen.next()
          val popValueId = valIdGen.next()
          val mlirTy = mlirType(ty)

          localSymEnv.put(name, popValueId)
          out.println(s"$popId, $popValueId = sigi.pop $envId: $mlirTy // $name")
          envId = popId
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

  def dumpAst(out: PrintStream)(t: TModule): Unit = {
    for ((_, fun) <- t.functions) {
      new MlirBuilder(out).dumpFunction(fun)
    }

    val mainFun = TFunDef("__main__", StackType(), t.mainExpr)
    new MlirBuilder(out).dumpFunction(mainFun)
  }

  def doDumpMlir(out: PrintStream)(in: Source): Unit = {

    val env = Env.Default.toTypingScope
    for {
      parsed <- KittenParser.parseFile(in)
      typed <- types.typeFile(env)(parsed)
    } dumpAst(out)(typed)

  }

  @main
  def dumpMlirMain(): Unit = doDumpMlir(System.out)(io.Source.stdin)


}
