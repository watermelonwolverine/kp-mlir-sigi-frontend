package de.cfaed.kitten


import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

package tokens {

  import scala.collection.immutable.Seq
  import scala.util.parsing.combinator.lexical.Scanners
  import scala.util.parsing.input.CharSequenceReader


  sealed class KToken

  case class NUMBER(value: Int) extends KToken
  case class ID(name: String) extends KToken
  case class OP(opName: String) extends KToken

  case object ARROW extends KToken
  case object IF extends KToken
  case object ELSE extends KToken
  case object ELIF extends KToken
  case object TRUE extends KToken
  case object FALSE extends KToken
  case object SEMI extends KToken
  case object COMMA extends KToken
  case object COLON extends KToken
  case object LPAREN extends KToken
  case object RPAREN extends KToken
  case object LBRACE extends KToken
  case object RBRACE extends KToken
  case object LBRACKET extends KToken
  case object RBRACKET extends KToken
  case object LANGLE extends KToken
  case object RANGLE extends KToken
  case class ERROR(msg: String) extends KToken


  object KittenLexer extends RegexParsers with Scanners {
    override def skipWhitespace: Boolean = true

    override type Token = KToken

    def ident = """[a-zA-Z]\w*""".r ^^ ID
    def op = "[-+*/%]|==|<>".r ^^ OP
    def number = """(0|[1-9]\d*)""".r ^^ { a => NUMBER(a.toInt) }

    def arrow = "->" ^^^ ARROW
    def colon = ":" ^^^ COLON
    def lparen = "(" ^^^ LPAREN
    def rparen = ")" ^^^ RPAREN
    def lbrace = "{" ^^^ LBRACE
    def rbrace = "}" ^^^ RBRACE
    def lbracket = "[" ^^^ LBRACKET
    def rbracket = "]" ^^^ RBRACKET
    def semi = ";" ^^^ SEMI
    def comma = "," ^^^ COMMA
    def langle = "<" ^^^ LANGLE
    def rangle = ">" ^^^ RANGLE

    private def keyword =
      "if" ^^^ IF
        | "else" ^^^ ELSE
        | "elif" ^^^ ELIF
        | "true" ^^^ TRUE
        | "false" ^^^ FALSE

    override def token: KittenLexer.Parser[KittenLexer.Token] =
      keyword | ident | number | arrow | colon | lparen | rparen
        | lbrace | rbrace | lbracket | rbracket
        | op | semi | comma | langle | rangle

    override def whitespace: KittenLexer.Parser[Any] = "\\s*".r

    override def errorToken(msg: String): KToken = ERROR(msg)
  }

  class KTokenScanner(in: String) extends KittenLexer.Scanner(CharSequenceReader(in))
}
