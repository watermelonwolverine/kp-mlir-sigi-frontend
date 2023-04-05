package de.cfaed.sigi

import ast.*
import repl.Env
import types.*

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.*

import scala.annotation.targetName
import org.scalatest.Inside.inside

import scala.util.Right

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
class ParserSpec extends AnyFunSuite with Matchers {

  inline def checkExpr(term: String, tree: KExpr): Unit = {
    test(term) {
      assertResult(Right(tree))(ast.SigiParser.parseExpr(term))
    }
  }

  inline def checkTreeMatches(term: String)(tree: PartialFunction[KStatement, Unit]): Unit = {
    test(term) {
      val parsed = ast.SigiParser.parseStmt(term)
      inside(parsed) {
        case Right(ast) => tree(ast)
      }
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

  import AstBuildingDsl.{*, given}

  checkExpr("1 2", p(1) ~ p(2))
  checkExpr("1 2 3", (p(1) ~ p(2)) ~ p(3))

  checkExpr("1 a + 2 a", ((p(1) ~ app("a")) ~ (p(2) ~ app("a"))) ~ app("+"))
  checkExpr("1 a + -2 a", ((p(1) ~ app("a")) ~ ((p(2) ~ app("unary_-")) ~ app("a"))) ~ app("+"))

  checkExpr("1 2 show", (p(1) ~ p(2)) ~ app("show"))
  checkExpr("{ show }", Quote(app("show")))
  checkExpr("\\show", Quote(app("show")))
  checkExpr("\\*", Quote(app("*")))
  checkExpr("true", p(true))
  checkExpr("false", p(false))
  checkExpr(""" "a" "b"  """, p("a") ~ p("b"))

  checkExpr("-> x, y; x y", names("x", "y") ~ (app("x") ~ app("y")))
  checkExpr("-> x;", names("x"))
  checkExpr("-> x, y; y", names("x", "y") ~ app("y"))

  checkExpr("if (true) 1 else 2",
    p(true) ~ (Quote(p(1)) ~ Quote(p(2)) ~ (app("cond") ~ app("apply")))
  )

  checkExpr("if 1 else 2",
    Quote(p(1)) ~ Quote(p(2)) ~ (app("cond") ~ app("apply"))
  )


  checkExpr("-> x, y; x * y", names("x", "y") ~ (app("x") ~ app("y") ~ app("*")))
  checkTreeMatches("func double(int-> int): ->x; 2*x;;") {
    case KFunDef("double", AFunType(List(_), List(_)), _) =>
  }

  checkTreeMatches("func id('a -> 'a): ->x; x;;") {
    case KFunDef("id", AFunType(List(ATypeVar("'a")), List(ATypeVar("'a"))), _) =>
  }


  checkTreeMatches("func id('a list -> 'b list): pop [];;") {
    case KFunDef("id", AFunType(List(ATypeCtor("list", List(ATypeVar("'a")))), _), _) =>
  }

  checkTreeMatches("func map('a list, ('a -> 'b) -> 'b list): pop pop [];;") {
    case KFunDef("map", AFunType(
    List(
    ATypeCtor("list", List(ATypeVar("'a"))),
    AFunType(List(ATypeVar("'a")), List(ATypeVar("'b")))
    ),
    List(ATypeCtor("list", List(ATypeVar("'b"))))), _) =>
  }

  checkTreeMatches("func id('S, 'a -> 'S, 'a): ->x; x;;") {
    case KFunDef("id", funT@AFunType(List(ARowVar("'S"), ATypeVar("'a")), List(ARowVar("'S"), ATypeVar("'a"))), _) =>
      val resolved = types.resolveFunType(Env.Default.toTypingScope)(funT)
      val expected = KFun(StackType.generic1(StackType.symmetric1))
      assertResult(Right(expected.toString))(resolved.map(_.toString))
  }
}
