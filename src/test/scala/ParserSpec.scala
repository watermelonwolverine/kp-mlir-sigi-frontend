package de.cfaed.sigi

import ast.*
import repl.Env
import types.*
import debug.given

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.*

import scala.annotation.targetName
import org.scalatest.Inside.inside

import scala.util.Right

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
class ParserSpec extends AnyFunSuite with Matchers {

  inline def checkExpr(term: String, tree: AstBuildingDsl.ExprMatcher): Unit = {
    test(term) {
      inside(new ast.SigiParser().parseExpr(term)) {
        case Right(ast) if tree.isDefinedAt(ast) => tree(ast)
      }
    }
  }

  inline def checkTreeMatches(term: String)(tree: PartialFunction[KStatement, Unit]): Unit = {
    test(term) {
      val parsed = new ast.SigiParser().parseStmt(term)
      inside(parsed) {
        case Right(ast) if tree.isDefinedAt(ast) => tree(ast)
      }
    }
  }

  object AstBuildingDsl {
    type ExprMatcher = PartialFunction[KExpr, Unit]

    given Conversion[KExpr, ExprMatcher] with
      override def apply(x: KExpr): ExprMatcher = { case e if e == x => }

    extension (e1: ExprMatcher)
      @targetName("chain")
      def ~(e2: ExprMatcher): ExprMatcher = {
        case Chain(a, b) if e1.isDefinedAt(a) && e2.isDefinedAt(b) =>
      }

    def p[T](t: T)(using KPrimitive[T]): PushPrim[T] = PushPrim(implicitly[KPrimitive[T]], t)

    def app(name: String): FunApply = FunApply(name)

    def names(names: String*): ExprMatcher = {
      case NameTopN(ids) if ids.map(_.sourceName) == names.toList =>
    }

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
  checkTreeMatches("let double: int-> int = ->x; 2*x;;") {
    case KFunDef(FuncId("double", _), Some(AFunType(List(_), List(_))), _) =>
  }

  checkTreeMatches("let id: 'a -> 'a = ->x; x;;") {
    case KFunDef(FuncId("id", _), Some(AFunType(List(ATypeVar("'a")), List(ATypeVar("'a")))), _) =>
  }


  checkTreeMatches("let id: 'a list -> 'b list = pop [];;") {
    case KFunDef(_, Some(AFunType(List(ATypeCtor("list", List(ATypeVar("'a")))), _)), _) =>
  }

  checkTreeMatches("let map: 'a list, ('a -> 'b) -> 'b list = pop pop [];;") {
    case KFunDef(_, Some(AFunType(
    List(
    ATypeCtor("list", List(ATypeVar("'a"))),
    AFunType(List(ATypeVar("'a")), List(ATypeVar("'b")))
    ),
    List(ATypeCtor("list", List(ATypeVar("'b")))))), _) =>
  }

  checkTreeMatches("let id: 'S, 'a -> 'S, 'a = ->x; x;;") {
    case KFunDef(_, Some(funT@AFunType(List(ARowVar("'S"), ATypeVar("'a")), List(ARowVar("'S"), ATypeVar("'a")))), _) =>
      val resolved = types.resolveFunType(Env.Default.toTypingScope)(funT)
      val expected = KFun(StackType.generic1(StackType.symmetric1))
      assertResult(Right(expected.toString))(resolved.map(_.toString))
  }

  checkTreeMatches("let id: 'S, ('S -> 'R, bool), ('R -> 'S) -> 'S = ->x; x;;") {
    case KFunDef(_, Some(funT), _) =>
      val resolved = types.resolveFunType(Env.Default.toTypingScope)(funT)
      assertResult(Right("('A, ('A -> 'B, bool), ('B -> 'A) -> 'A)"))(resolved.map(_.toString))
  }

  checkTreeMatches("let id_inferred = ->x; x;;") {
    case f@KFunDef(_, None, _) =>
      types.doValidation(Env.Default.toTypingScope)(f) match
        case Right(TFunDef(_, StackType(List(a: KTypeVar), List(b: KTypeVar)), _, _)) if a == b =>
        case a => fail(s"Unexpected result: $a")
  }
}
