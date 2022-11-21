

package de.cfaed.kitten

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}


package ast {

  import tokens.*
  import types.{KDataType, KFun, KPrimitive, StackType}

  import de.cfaed.kitten.tokens

  /**
    * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
    */
  sealed trait KNode

  sealed trait KStatement extends KNode
  case class KBlock(stmts: List[KStatement]) extends KStatement
  case class KFunDef(name: String, ty: FunType, body: KExpr) extends KStatement
  case class KExprStatement(e: KExpr) extends KStatement

  /** The result of parsing a type. Some of the components may be unresolved. */
  sealed trait TypeSpec
  case class TypeCtor(name: String, tyargs: List[TypeSpec]=Nil) extends TypeSpec
  case class FunType(typarms: List[String], consumes: List[TypeSpec], produces: List[TypeSpec]) extends TypeSpec

  sealed trait KExpr extends KNode {
    override def toString: String = this match
      case Chain(a, b) => s"($a $b)"
      case FunApply(name) => if Character.isAlphabetic(name(0)) then name else s"($name)"
      case PushPrim(_, value) => value.toString
      case NameTopN(names) => names.mkString("-> ", ", ", ";")
      case PushList(items) => items.mkString("[", ", ", "]")
      case Quote(term) => s"{ $term }"
  }

  case class Chain(a: KExpr, b: KExpr) extends KExpr
  case class PushPrim[T](ty: KPrimitive[T], value: T) extends KExpr
  case class PushList(items: List[KExpr]) extends KExpr
  object PushPrim {
    val PushTrue: PushPrim[Boolean] = PushPrim(types.KBool, true)
    val PushFalse: PushPrim[Boolean] = PushPrim(types.KBool, false)
  }
  case class FunApply(name: String) extends KExpr
  case class Quote(term: KExpr) extends KExpr
  case class NameTopN(names: List[String]) extends KExpr {
    def stackType: StackType = StackType.generic(newTVar => {
      StackType(consumes = names.map(_ => newTVar()))
    })
  }


  object KittenParser extends Parsers with PackratParsers {
    override type Elem = KToken

    private def thunk =
      LBRACE ~> (exprSeq | accept("operator", { case OP(n) => FunApply(n) })) <~ RBRACE ^^ Quote

    private def dty: Parser[TypeSpec] =
      id ~ tyArgs.? ^^ { case name ~ tyargs => TypeCtor(name, tyargs.getOrElse(Nil)) }
        | LPAREN ~> funTy <~ RPAREN // funtype

    // note that the normal kitten grammar uses angle brackets
    private def tyArgs: Parser[List[TypeSpec]] = LBRACKET ~> rep1sep(dty, COMMA) <~ RBRACKET
    private def tyParms: Parser[List[String]] = LBRACKET ~> rep1sep(id, COMMA) <~ RBRACKET

    private def funTy: Parser[FunType] =
      tyParms.? ~ repsep(dty, COMMA) ~ ARROW ~ repsep(dty, COMMA) ^^ {
        case typarms ~ consumes ~ _ ~ produces => FunType(typarms.getOrElse(Nil), consumes, produces)
      }

    private def inParens[P](p: Parser[P]): Parser[P] = LPAREN ~> p <~ RPAREN

    private def funDef: Parser[KFunDef] =
    // note that the normal kitten grammar does not use a semi
      DEFINE ~> id ~ inParens(funTy) ~ COLON ~ exprSeq <~ PHAT_SEMI ^^ {
        case name ~ ty ~ _ ~ body => KFunDef(name, ty, body)
      }

    private def parexpr = LPAREN ~> exprSeq <~ RPAREN

    private def opAsFunApply = accept("operator", { case OP(n) => FunApply(n) })
    private def id: Parser[String] = accept("identifier", { case ID(n) => n })
    private def identAsFunApply = id ^^ FunApply

    private def ifelse: Parser[KExpr] =
      (IF ~> parexpr.? ~ thunk
        ~ rep(ELIF ~> parexpr ~ thunk)
        ~ (ELSE ~> thunk)
        ) ^^ {
        case (cond: Option[KExpr]) ~ (thenThunk: Quote) ~ (elifs: List[KExpr ~ Quote]) ~ (elseThunk: Quote) =>
          def makeIf(thenThunk: Quote, elseThunk: Quote): KExpr =
            Chain(Chain(thenThunk, elseThunk), FunApply(builtins.Intrinsic_if))

          val foldedElseIf = elifs.foldRight[Quote](elseThunk) {
            case (c ~ t, f) => Quote(Chain(c, makeIf(t, f)))
          }
          val ifelse = makeIf(thenThunk, foldedElseIf)

          cond.map(c => Chain(c, ifelse)).getOrElse(ifelse)
      }

    private def primary: Parser[KExpr] =
      TRUE ^^^ PushPrim.PushTrue
        | FALSE ^^^ PushPrim.PushFalse
        | identAsFunApply
        | accept("number", { case NUMBER(v) => PushPrim(types.KInt, v) })
        | accept("string", { case STRING(v) => PushPrim(types.KString, v) })
        | BACKSLASH ~> (opAsFunApply | identAsFunApply) ^^ Quote
        | (LBRACKET ~> repsep(exprSeq, COMMA) <~ RBRACKET ^^ PushList)
        | (LPAREN ~> (exprSeq | opAsFunApply) <~ RPAREN)
        | thunk
        | (ARROW ~> rep1sep(accept("identifier", { case ID(name) => name }), COMMA) <~ SEMI ^^ NameTopN)
        | ifelse

    private def unary: Parser[KExpr] =
      (OP("-") | OP("+") | OP("~")).? ~ primary ^^ {
        case Some(OP(op)) ~ e => Chain(e, FunApply("unary_" + op))
        case None ~ e => e
      }

    private def multexpr: Parser[KExpr] =
      unary ~ rep((OP("*") | OP("/") | OP("%")) ~ unary) ^^ {
        case e1 ~ list => list.foldLeft(e1) {
          case (a, OP(op) ~ b) => Chain(Chain(a, b), FunApply(op))
        }
      }

    private def addexpr: Parser[KExpr] =
      multexpr ~ rep((OP("+") | OP("-")) ~ multexpr) ^^ {
        case e1 ~ list => list.foldLeft(e1) {
          case (a, OP(op) ~ b) => Chain(Chain(a, b), FunApply(op))
        }
      }


    private def sequenceableExpr: Parser[KExpr] =
      addexpr

    private def exprSeq: Parser[KExpr] =
      rep1(sequenceableExpr) ^^ (_ reduceLeft Chain.apply)

    private def expr: Parser[KExpr] = exprSeq
    private def statement: Parser[KStatement] = funDef | expr <~ PHAT_SEMI.? ^^ KExprStatement
    private def statementList: Parser[KBlock] = rep(statement) ^^ KBlock

    def apply(source: String): Either[KittenParseError, KExpr] = apply(source, expr)

    def apply[T](source: String, parser: Parser[T]): Either[KittenParseError, T] = {
      val reader = new KTokenScanner(source)
      parser(reader) match {
        case NoSuccess(msg, _) => Left(KittenParseError(msg))
        case Success(result, input) =>
          if (input.atEnd) Right(result)
          else Left(KittenParseError(s"Unparsed tokens: ${source.substring(math.max(0, input.pos.column - 1))}"))
      }
    }

    private def validate(e: KStatement): Option[KittenParseError] = e match
      case KBlock(stmts) => stmts.foldLeft[Option[KittenParseError]](None)((a, b) => a.orElse(validate(b)))
      case KFunDef(_, _, body) => validate(body)
      case KExprStatement(e) => validate(e)

    private def validate(e: KExpr): Option[KittenParseError] = e match
      case Chain(a, b) => validate(a).orElse(validate(b))
      case Quote(term) => validate(term)
      case node@NameTopN(names) =>
        if names.distinct.lengthCompare(names) != 0 then
          Some(KittenParseError.namesShouldBeUnique(node))
        else
          None
      case _ => None


    def parseExpr(code: String): Either[KittenCompilationError, KExpr] = {
      KittenParser(code, expr).flatMap(e => validate(e).toLeft(e))
    }

    def parseStmt(code: String): Either[KittenCompilationError, KStatement] = {
      KittenParser(code, statementList).flatMap(e => validate(e).toLeft(e))
    }

  }


}
