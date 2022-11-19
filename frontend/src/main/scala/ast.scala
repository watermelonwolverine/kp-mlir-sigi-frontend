

package de.cfaed.kitten

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}


package ast {

  import tokens._

  /**
   * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
   */
  sealed trait KNode

  sealed trait KExpr extends KNode {
    override def toString: String = this match
      case Chain(a, b) => s"($a $b)"
      case FunApply(name) => if Character.isAlphabetic(name(0)) then name else s"($name)"
      case Number(value) => value.toString
      case NameTop(name) => s"-> $name"
  }

  case class Chain(a: KExpr, b: KExpr) extends KExpr
  case class Number(value: Int) extends KExpr
  case class FunApply(name: String) extends KExpr
  case class NameTop(name: String) extends KExpr


  object KittenParser extends Parsers with PackratParsers {
    override type Elem = KToken

    def expr: Parser[KExpr] =
      accept("identifier", { case ID(name) => FunApply(name) })
        | accept("number", { case NUMBER(v) => Number(v) })
        | (LPAREN ~ (exprSeq | accept("operator", { case OP(n) => FunApply(n) })) ~ RPAREN ^^ { case _ ~ e ~ _ => e })
        | (ARROW ~> accept("id", { case ID(name) => NameTop(name) }))

    def exprSeq: Parser[KExpr] =
      rep1(expr) ^^ (_ reduceLeft Chain.apply)

    def apply(source: String): Either[KittenParseError, KExpr] = {
      val reader = new KTokenScanner(source)
      exprSeq(reader) match {
        case NoSuccess(msg, _) => Left(KittenParseError(msg))
        case Success(result, input) =>
          if (input.atEnd) Right(result)
          else Left(KittenParseError(s"Unparsed tokens: ${source.substring(math.max(0, input.pos.column - 1))}"))
      }
    }
  }


  case class ParsedCode(source: String, ast: KExpr)

  def parse(code: String): Either[KittenCompilationError, ParsedCode] = {
    KittenParser(code).map(e => ParsedCode(code, e))
  }

}
