package de.cfaed.kitten

import ast.*
import datamodel.{TypeDescriptor, TypeParm}
import types.*
import types.StackType.canonicalize

import de.cfaed.kitten.eval.Env

import java.io.PrintStream
import scala.io
import scala.annotation.tailrec

package dumpmlir {

  sealed trait MlirValue {

  }

  case class MlirIdent(id: Int) extends MlirValue {
    override def toString: String = "%" + id
  }

  class MlirBuilder(val out: PrintStream) {
    private var mlirId = MlirIdent(0)

    def nextId(): MlirIdent = {
      val cur = mlirId
      mlirId = MlirIdent(cur.id + 1)
      cur
    }

    def mlirType(ty: KStackTypeItem): String = ty match
      case KInt => "!sigi.int"
      case KBool => "!sigi.bool"
      case KString => "!sigi.str"
      case KFun(stack) =>
        val consumed = stack.consumes.map(mlirType).mkString(",")
        val produced = stack.produces.map(mlirType).mkString(",")
        s"!sigi.fun<[$consumed], [$produced]>"
      case KList(item) => s"!sigi.list<${mlirType(item)}>"
      case _ => throw new IllegalArgumentException(s"Cannot emit type with variable $ty")

    def makeConstAttr[T](ty: KPrimitive[T], value: T): String = ty match
      case KInt => value.toString
      case KBool => value.toString
      case KString => s"\"$value\""

    def dumpAst(t: TypedExpr): Unit = t match
      case TChain(stackTy, a, b) =>
        dumpAst(a)
        dumpAst(b)

      case TPushList(ty, items) =>
        val lastId = mlirId
        val itemIds = for (item <- items) yield {
          dumpAst(item)
          mlirId
        }
        val args = (lastId :: itemIds).mkString(", ")
        val listId = nextId()
        val pushId = nextId()

        val mlirTy = mlirType(ty)

        out.println(s"$listId = sigi.create_list $args: $mlirTy")
        out.println(s"$pushId = sigi.push $listId: $mlirTy")

      case TPushPrim(ty, value) =>
        val lastId = mlirId
        val cst = nextId()
        val push = nextId()

        val cstAttr = makeConstAttr(ty, value)
        val mlirTy = mlirType(ty)

        out.println(s" $cst = sigi.constant $cstAttr: $mlirTy")
        out.println(s" $push = sigi.push $lastId $cst: $mlirTy")


      case TFunApply(stackTy, name) => ???
      // here we need the environment to associate this with a reused function.
      // The frontend needs to do more work:
      // - monomorphize everything bzw erase types
      // - resolve functions to actual definitions and give them mangled idents based on their scope.
      //
      // builtins could be mapped to MLIR dialect operators


      case TPushQuote(term) => ???


      case TNameTopN(stackTy, names) => ???
  }

  def dumpAst(out: PrintStream)(t: TypedStmt): Unit = {


  }

  @main
  def main(): Unit = {
    val source = io.Source.stdin.getLines().mkString("\n")

    val env = Env.Default
    for {
      parsed <- KittenParser.parseStmt(source)
      typed <- eval.doValidation(Env.Default)(parsed)
    } yield {
      dumpAst(System.out)(typed)
    }

  }


}
