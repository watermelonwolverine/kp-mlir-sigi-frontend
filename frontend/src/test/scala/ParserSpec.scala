package de.cfaed.kitten

import ast.*
import eval.Env
import types.{KPrimitive, StackType, TypingScope}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.*

import scala.annotation.targetName

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

  object AstBuildingDsl {

    extension (e1: KExpr)
      @targetName("chain")
      def ~(e2: KExpr): Chain = Chain(e1, e2)

    def p[T](t: T)(using KPrimitive[T]): PushPrim[T] = PushPrim(implicitly[KPrimitive[T]], t)
    def app(name:String): FunApply = FunApply(name)
    def names(names: String*): NameTopN = NameTopN(names.toList)

    given KPrimitive[Int] = types.KInt
    given KPrimitive[String] = types.KString
    given KPrimitive[Boolean] = types.KBool
  }

  import AstBuildingDsl.*
  import AstBuildingDsl.given

  checkExpr("1 2", p(1) ~ p(2))
  checkExpr("1 2 3", (p(1) ~ p(2)) ~ p(3))
  checkExpr("1 2 show", (p(1) ~ p(2)) ~ app("show"))
  checkExpr("{ show }", Quote(app("show")))
  checkExpr("\\show", Quote(app("show")))
  checkExpr("\\*", Quote(app("*")))
  checkExpr("true", p(true))
  checkExpr("false", p(false))
  checkExpr(""" "a" "b"  """, p("a") ~ p("b"))

  checkExpr("-> x, y; x y", names("x", "y") ~ app("x") ~ app("y"))
  checkExpr("-> x;", names("x"))
  checkExpr("-> x, y; y", names("x", "y") ~ app("y"))

  checkExpr("if (true) { 1 } else { 2 }",
    p(true) ~ (Quote(p(1)) ~ Quote(p(2)) ~ app(builtins.Intrinsic_if))
  )

  checkExpr("if { 1 } else { 2 }",
    Quote(p(1)) ~ Quote(p(2)) ~ app(builtins.Intrinsic_if)
  )


  checkExpr("-> x, y; x * y", names("x", "y") ~ (app("x") ~ app("y") ~ app("*")))
  checkTreeMatches("define double(int-> int): ->x; 2*x;;") {
    case KBlock(List(KFunDef("double", FunType(Nil, List(_), List(_)), _))) =>
  }
}
