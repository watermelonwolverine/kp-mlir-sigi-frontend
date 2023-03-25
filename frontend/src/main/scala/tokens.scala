package de.cfaed.sigi


import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

package tokens {

  import scala.collection.immutable.Seq
  import scala.util.parsing.combinator.lexical.Scanners
  import scala.util.parsing.input.CharSequenceReader
  import scala.util.parsing.input.Positional


  sealed class KToken extends Positional

  case class NUMBER(value: Int) extends KToken
  case class ID(name: String) extends KToken
  case class OP(opName: String) extends KToken
  case class STRING(value: String) extends KToken

  case object ARROW extends KToken
  case object IF extends KToken
  case object ELSE extends KToken
  case object ELIF extends KToken
  case object TRUE extends KToken
  case object FALSE extends KToken
  case object DEFINE extends KToken
  case object SEMI extends KToken
  case object PHAT_SEMI extends KToken
  case object COMMA extends KToken
  case object COLON extends KToken
  case object BACKSLASH extends KToken
  case object LPAREN extends KToken
  case object RPAREN extends KToken
  case object LBRACE extends KToken
  case object RBRACE extends KToken
  case object LBRACKET extends KToken
  case object RBRACKET extends KToken
  case object LANGLE extends KToken
  case object RANGLE extends KToken
  case class ERROR(msg: String) extends KToken


  object SigiLexer extends RegexParsers with Scanners {
    override def skipWhitespace: Boolean = true

    override type Token = KToken

    def ident: Parser[ID] = """[a-zA-Z]\w*""".r ^^ ID.apply
    def op: Parser[OP] = "[-+*/%]|==|<>".r ^^ OP.apply
    def number: Parser[NUMBER] = """(0|[1-9]\d*)""".r ^^ { a => NUMBER(a.toInt) }
    def string: Parser[STRING] =
      """"([^\\"]*+|\\[\\rn"])*"""".r ^^ {
        str =>
          val woDelim = str.substring(1, str.length - 1)
          val unescaped = "\\[rn]".r.replaceAllIn(woDelim,
            m => m.matched.charAt(1) match
              case 'r' => "\n"
              case 'n' => "\n"
              case '"' => "\"")
          STRING(unescaped)
      }

    private def punct =
    "->" ^^^ ARROW
    | ":" ^^^ COLON
    | "(" ^^^ LPAREN
    | ")" ^^^ RPAREN
    | "{" ^^^ LBRACE
    | "}" ^^^ RBRACE
    | "[" ^^^ LBRACKET
    | "]" ^^^ RBRACKET
    | ";;" ^^^ PHAT_SEMI
    | ";" ^^^ SEMI
    | "," ^^^ COMMA
    | "<" ^^^ LANGLE
    | ">" ^^^ RANGLE
    | "\\" ^^^ BACKSLASH

    private def keyword =
      "if" ^^^ IF
        | "else" ^^^ ELSE
        | "elif" ^^^ ELIF
        | "true" ^^^ TRUE
        | "false" ^^^ FALSE
        | "define" ^^^ DEFINE

    override def token: SigiLexer.Parser[SigiLexer.Token] =
      positioned(keyword | ident | number | punct | op | string)

    override def whitespace: SigiLexer.Parser[Any] = "\\s*".r

    override def errorToken(msg: String): KToken = ERROR(msg)
  }

  class KTokenScanner(in: String) extends SigiLexer.Scanner(CharSequenceReader(in))
}
