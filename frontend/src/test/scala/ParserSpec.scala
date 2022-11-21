package de.cfaed.kitten

import ast.*
import eval.Env
import types.{StackType, TypingScope}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should._

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
class ParserSpec extends AnyFunSuite with Matchers {

  inline def checkExpr(term: String, tree: KExpr): Unit = {
    test(term) {
      assertResult(Right(tree))(ast.KittenParser.parseExpr(term))
    }
  }

  inline def checkTreeMatches(term: String)(tree: PartialFunction[Any, Unit]): Unit = {
    test(term) {
      ast.KittenParser.parseStmt(term).right.get should matchPattern(tree)
    }
  }

  private def number(i: Int) = PushPrim(types.KInt, i)

  checkExpr("1 2", Chain(number(1), number(2)))
  checkExpr("1 2 3", Chain(Chain(number(1), number(2)), number(3)))
  checkExpr("1 2 show", Chain(Chain(number(1), number(2)), FunApply("show")))
  checkExpr("{ show }", Quote(FunApply("show")))
  checkExpr("\\show", Quote(FunApply("show")))
  checkExpr("\\*", Quote(FunApply("*")))
  checkExpr("true", PushPrim.PushTrue)
  checkExpr("false", PushPrim.PushFalse)
  checkExpr(""" "a" "b"  """, Chain(PushPrim(types.KString, "a"), PushPrim(types.KString, "b")))

  checkExpr("-> x, y; x y", Chain(Chain(NameTopN(List("x", "y")), FunApply("x")), FunApply("y")))
  checkExpr("-> x;", NameTopN(List("x")))
  checkExpr("-> x, y; y", Chain(NameTopN(List("x", "y")), FunApply("y")))

  checkExpr("-> x, y; x * y", Chain(NameTopN(List("x", "y")), Chain(Chain(FunApply("x"), FunApply("y")), FunApply("*"))))
  checkTreeMatches("define double(int-> int): ->x; 2*x;;"){
    case KBlock(List(KFunDef("double", FunType(Nil, List(_), List(_)), _))) =>
  }
}
