

package de.cfaed.sigi


package ast {

  import scala.annotation.tailrec
  import scala.util.matching.Regex
  import scala.util.parsing.combinator.{PackratParsers, Parsers, RegexParsers}
  import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}
  import tokens.*
  import types.{KDataType, KFun, KPrimitive, StackType, TypedExpr}

  import de.cfaed.sigi.tokens

  import scala.io.Source

  /**
    * An AST node before semantic analysis.
    *
    * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
    */
  sealed trait KNode extends Positional

  case class KFile(funs: List[KFunDef], mainExpr: KExpr)


  sealed trait KStatement extends KNode
  case class KBlock(stmts: List[KStatement]) extends KStatement
  case class KFunDef(name: String, ty: AFunType, body: KExpr) extends KStatement
  case class KExprStatement(e: KExpr) extends KStatement {
    setPos(e.pos)
  }


  /** The result of parsing a type. Some of the components may be unresolved.
    * Semantic analysis lifts this into a [[types.KStackTypeItem]] in [[types.resolveType]].
    */
  sealed trait AstType extends Positional
  case class ATypeCtor(name: String, tyargs: List[AstType] = Nil) extends AstType
  case class ATypeVar(name: String) extends AstType
  case class ARowVar(name: String) extends AstType
  case class AFunType(consumes: List[AstType], produces: List[AstType]) extends AstType

  sealed trait KExpr extends KNode {
    override def toString: String = this match
      case Chain(a, b) => s"($a $b)"
      case FunApply(name) => if Character.isAlphabetic(name(0)) then name else s"($name)"
      case PushPrim(_, value) => value.toString
      case NameTopN(names) => names.mkString("-> ", ", ", ";")
      case PushList(items) => items.mkString("[", ", ", "]")
      case Quote(term) => s"{ $term }"
      case OpaqueExpr(te) => s"(${te.erase.toString} : ${te.stackTy})"
  }

  case class Chain(a: KExpr, b: KExpr) extends KExpr {
    setPos(a.pos)
  }

  case class PushPrim[T](ty: KPrimitive[T], value: T) extends KExpr

  case class PushList(items: List[KExpr]) extends KExpr

  case class FunApply(name: String) extends KExpr

  /** This is an already typed expr wrapped as a kexpr.
    * Used in the repl to chain partial typing results.
    */
  case class OpaqueExpr(te: TypedExpr) extends KExpr {
    setPos(te.pos)
  }

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
      LBRACE ~> expr <~ RBRACE ^^ Quote.apply

    private def dty_atom: Parser[AstType] =
      id ^^ { id => ATypeCtor(id.name, Nil).setPos(id.pos) }
        | LPAREN ~> (funTy | dty) <~ RPAREN
        | accept("type variable", { case t: TVAR => ATypeVar(t.name).setPos(t.pos) })
        | accept("row variable", { case t: ROWVAR => ARowVar(t.name).setPos(t.pos) })

    private def dty: Parser[AstType] =
      dty_atom ~ rep(dty_atom) ^? ( {
        case hd ~ Nil => hd
        case hd ~ (args :+ (last@ATypeCtor(name, Nil))) =>
          ATypeCtor(name, hd :: args).setPos(last.pos)
      }, _ => "Type constructor should be an identifier")

    private def funTy: Parser[AFunType] =
      repsep(dty, COMMA) ~ ARROW ~ repsep(dty, COMMA) ^^ {
        case consumes ~ _ ~ produces => AFunType(consumes, produces)
      }

    private def inParens[P](p: Parser[P]): Parser[P] = LPAREN ~> p <~ RPAREN

    private def funDef: Parser[KFunDef] =
    // note that the normal sigi grammar does not use a semi
      DEFINE ~> id ~ inParens(funTy) ~ COLON ~ expr <~ PHAT_SEMI ^^ {
        case id ~ ty ~ _ ~ body => KFunDef(id.name, ty, body).setPos(id.pos)
      }

    private def parexpr = LPAREN ~> expr <~ RPAREN

    private def opAsFunApply = accept("operator", { case op: OP => FunApply(op.opName).setPos(op.pos) })
    private def id: Parser[ID] = accept("identifier", { case id: ID => id })
    private def identAsFunApply = id ^^ { id => FunApply(id.name).setPos(id.pos) }

    private def ifelse: Parser[KExpr] =
      (IF ~> parexpr.? ~ (expr ^^ Quote.apply)
        ~ rep(ELIF ~> parexpr ~ (expr ^^ Quote.apply))
        ~ (ELSE ~> expr ^^ Quote.apply)
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
        | (LBRACKET ~ repsep(expr, COMMA) <~ RBRACKET ^^ { case bracket ~ list => PushList(list).setPos(bracket.pos) })
        | (LPAREN ~> (expr | opAsFunApply) <~ RPAREN)
        | thunk
        | ifelse
    private def nameTopN: Parser[KExpr] =
       ARROW ~ rep1sep(id, COMMA) <~ SEMI ^^ { case arrow ~ ids => NameTopN(ids.map(_.name)).setPos(arrow.pos) }

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

    private def exprSeq: Parser[KExpr] =
      unary ~ rep(primary) ^^ {
        case fst ~ rest => (fst :: rest).reduceLeft(Chain.apply)
      }

    private def multexpr: Parser[KExpr] = makeBinary(exprSeq, OP("*") | OP("/") | OP("%"))

    private def addexpr: Parser[KExpr] = makeBinary(multexpr, OP("+") | OP("-"))

    private def compexpr: Parser[KExpr] = makeBinary(addexpr, OP("<") | OP(">") | OP("<=") | OP(">=") | OP("=") | OP("<>"))

    private def expr: Parser[KExpr] = rep1(compexpr | nameTopN) ^^ (_ reduceLeft Chain.apply)

    private def statement: Parser[KStatement] = funDef | expr <~ PHAT_SEMI.? ^^ KExprStatement.apply

    private def statementList: Parser[KStatement] = rep(statement) ^^ {
      case List(st) => st
      case stmts => KBlock(stmts)
    }

    private def file: Parser[KFile] = rep(funDef) ~ expr <~ PHAT_SEMI.? ^^ {
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

    // these validation methods only perform syntactic validation.

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
