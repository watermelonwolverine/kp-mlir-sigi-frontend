

package de.cfaed.sigi


package ast {

  import scala.annotation.tailrec
  import scala.util.matching.Regex
  import scala.util.parsing.combinator.{PackratParsers, Parsers, RegexParsers}
  import scala.util.parsing.input.{NoPosition, Position, Reader, Positional}

  import tokens.*
  import types.{KDataType, KFun, KPrimitive, StackType}

  import de.cfaed.sigi.tokens

  import scala.io.Source

  /**
    * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
    */
  sealed trait KNode extends Positional

  case class KFile(funs: List[KFunDef], mainExpr: KExpr)


  sealed trait KStatement extends KNode
  case class KBlock(stmts: List[KStatement]) extends KStatement
  case class KFunDef(name: String, ty: FunType, body: KExpr) extends KStatement
  case class KExprStatement(e: KExpr) extends KStatement {
    setPos(e.pos)
  }

  /** The result of parsing a type. Some of the components may be unresolved. */
  sealed trait TypeSpec extends Positional
  case class TypeCtor(name: String, tyargs: List[TypeSpec] = Nil) extends TypeSpec
  case class FunType(consumes: List[TypeSpec], produces: List[TypeSpec]) extends TypeSpec

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
  case class FunApply(name: String) extends KExpr
  case class Quote(term: KExpr) extends KExpr {
    setPos(term.pos)
  }
  case class NameTopN(names: List[String]) extends KExpr {
    def stackType: StackType = StackType.generic(newTVar => {
      StackType(consumes = names.map(_ => newTVar()))
    })
  }


  object SigiParser extends Parsers with PackratParsers {
    override type Elem = KToken

    private def thunk =
      LBRACE ~> (exprSeq | accept("operator", { case OP(n) => FunApply(n) })) <~ RBRACE ^^ Quote.apply

    private def dty: Parser[TypeSpec] =
      id ~ tyArgs.? ^^ { case id ~ tyargs => TypeCtor(id.name, tyargs.getOrElse(Nil)).setPos(id.pos) }
        | LPAREN ~> funTy <~ RPAREN // funtype

    // note that the normal sigi grammar uses angle brackets
    // todo maybe use OCaml postfix syntax for type ctors
    //  although maybe using familiar looking generic types makes it look more familiar.
    // todo maybe remove type param clauses, they can be defined implicitly
    private def tyArgs: Parser[List[TypeSpec]] = LBRACKET ~> rep1sep(dty, COMMA) <~ RBRACKET
    // todo currently not possible to write a generic type explicitly.

    private def funTy: Parser[FunType] =
      repsep(dty, COMMA) ~ ARROW ~ repsep(dty, COMMA) ^^ {
        case consumes ~ _ ~ produces => FunType(consumes, produces)
      }

    private def inParens[P](p: Parser[P]): Parser[P] = LPAREN ~> p <~ RPAREN

    private def funDef: Parser[KFunDef] =
    // note that the normal sigi grammar does not use a semi
      DEFINE ~> id ~ inParens(funTy) ~ COLON ~ exprSeq <~ PHAT_SEMI ^^ {
        case id ~ ty ~ _ ~ body => KFunDef(id.name, ty, body).setPos(id.pos)
      }

    private def parexpr = LPAREN ~> exprSeq <~ RPAREN

    private def opAsFunApply = accept("operator", { case op: OP => FunApply(op.opName).setPos(op.pos) })
    private def id: Parser[ID] = accept("identifier", { case id: ID => id })
    private def identAsFunApply = id ^^ { id => FunApply(id.name).setPos(id.pos) }

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
      TRUE ^^ { tok => PushPrim(types.KBool, true).setPos(tok.pos) }
        | FALSE ^^ { tok => PushPrim(types.KBool, false).setPos(tok.pos) }
        | identAsFunApply
        | accept("number", { case n@NUMBER(v) => PushPrim(types.KInt, v).setPos(n.pos) })
        | accept("string", { case s@STRING(v) => PushPrim(types.KString, v).setPos(s.pos) })
        | BACKSLASH ~> (opAsFunApply | identAsFunApply) ^^ Quote.apply
        | (LBRACKET ~ repsep(exprSeq, COMMA) <~ RBRACKET ^^ { case bracket ~ list => PushList(list).setPos(bracket.pos) })
        | (LPAREN ~> (exprSeq | opAsFunApply) <~ RPAREN)
        | thunk
        | (ARROW ~ rep1sep(id, COMMA) <~ SEMI ^^ { case arrow ~ ids => NameTopN(ids.map(_.name)).setPos(arrow.pos) })
        | ifelse

    private def unary: Parser[KExpr] =
      (OP("-") | OP("+") | OP("~")).? ~ primary ^^ {
        case Some(op: OP) ~ e => Chain(e, FunApply("unary_" + op.opName).setPos(op.pos))
        case None ~ e => e
      }

    private def makeBinary(lowerPrecParser: Parser[KExpr],
                           opParser: Parser[KToken]): Parser[KExpr] =
      lowerPrecParser ~ rep(opParser ~ lowerPrecParser) ^^ {
        case e1 ~ list => list.foldLeft(e1) {
          case (a, (op: OP) ~ b) => Chain(Chain(a, b), FunApply(op.opName).setPos(op.pos)).setPos(op.pos)
        }
      }

    private def multexpr: Parser[KExpr] = makeBinary(unary, OP("*") | OP("/") | OP("%"))

    private def addexpr: Parser[KExpr] = makeBinary(multexpr, OP("+") | OP("-"))


    private def sequenceableExpr: Parser[KExpr] =
      addexpr

    private def exprSeq: Parser[KExpr] =
      rep1(sequenceableExpr) ^^ (_ reduceLeft Chain.apply)

    private def expr: Parser[KExpr] = exprSeq
    private def statement: Parser[KStatement] = funDef | expr <~ PHAT_SEMI.? ^^ KExprStatement.apply
    private def statementList: Parser[KBlock] = rep(statement) ^^ KBlock.apply
    private def file: Parser[KFile] = rep(funDef) ~ expr ^^ {
      case (funs: List[KFunDef]) ~ (expr: KExpr) => KFile(funs, expr)
    }

    def apply(source: String): Either[SigiParseError, KExpr] = apply(source, expr)

    def apply[T](source: String, parser: Parser[T]): Either[SigiParseError, T] = {
      val reader = new KTokenScanner(source)
      parser(reader) match {
        case NoSuccess(msg, input) => Left(SigiParseError(msg, input.pos))
        case Success(result, input) =>
          if (input.atEnd) Right(result)
          else Left(SigiParseError(s"Unparsed tokens: ${source.substring(math.max(0, input.pos.column - 1))}", input.pos))
      }
    }

    private def validate(e: KStatement): Option[SigiParseError] = e match
      case KBlock(stmts) => stmts.foldLeft[Option[SigiParseError]](None)((a, b) => a.orElse(validate(b)))
      case KFunDef(_, _, body) => validate(body)
      case KExprStatement(e) => validate(e)

    private def validate(file: KFile): List[SigiParseError] =
      (file.funs.map(validate) ++ List(validate(file.mainExpr))).collect { case Some(err) => err }

    private def validate(e: KExpr): Option[SigiParseError] = e match
      case Chain(a, b) => validate(a).orElse(validate(b))
      case Quote(term) => validate(term)
      case node@NameTopN(names) =>
        if names.distinct.lengthCompare(names) != 0 then
          Some(SigiParseError.namesShouldBeUnique(node))
        else
          None
      case _ => None


    def parseExpr(code: String): Either[SigiCompilationError, KExpr] = {
      SigiParser(code, expr).flatMap(e => validate(e).toLeft(e))
    }

    def parseStmt(code: String): Either[SigiCompilationError, KStatement] = {
      SigiParser(code, statementList).flatMap(e => validate(e).toLeft(e))
    }

    def parseFile(fileContents: String): Either[SigiCompilationError, KFile] = {
      SigiParser(fileContents, file).flatMap(file => {
        val errors = validate(file)
        if errors.isEmpty
        then Right(file)
        else Left(SigiCompilationError.allOf(errors))
      })
    }


  }


}
