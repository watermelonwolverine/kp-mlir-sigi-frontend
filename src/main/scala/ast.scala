

package de.cfaed.sigi


package ast {

  import scala.annotation.tailrec
  import scala.util.matching.Regex
  import scala.util.parsing.combinator.{PackratParsers, Parsers, RegexParsers}
  import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}
  import tokens.*
  import types.{KDataType, KFun, KPrimitive, StackType, TypedExpr, TypingScope}

  import de.cfaed.sigi.builtins.BuiltinFunSpec
  import de.cfaed.sigi.tokens

  import java.util.Comparator
  import scala.io.Source

  case class FilePos(position: Position, fileName: String)

  /**
    * An AST node before semantic analysis.
    *
    * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
    */
  sealed trait KNode extends Positional

  case class KFile(sourceFileName: String, funs: List[KFunDef], mainExpr: KExpr)


  implicit object FuncIdOrdering extends Ordering[FuncId] {
    override def compare(x: FuncId, y: FuncId): Int =
      (x, y) match
        // builtins come first
        case (BuiltinFuncId(a), BuiltinFuncId(b)) => a.compareTo(b)
        case (BuiltinFuncId(_), _) => -1
        case (_, BuiltinFuncId(_)) => 1
        case (a: PositionedFuncId, b: PositionedFuncId) =>
          if a.filePos.position < b.filePos.position then -1
          else if a.filePos.position == b.filePos.position then a.sourceName.compareTo(b.sourceName)
          else 1
        case (FuncInstantiationId(a, _), FuncInstantiationId(b, _)) => compare(a, b)
        case (FuncInstantiationId(orig, _), other) if compare(orig, other) != 0 => compare(orig, other)
        case (other, FuncInstantiationId(orig, _)) if compare(orig, other) != 0 => compare(orig, other)
        case (_, _) => 0 // stable sorts wouldn't eliminate equal elements

  }

  /** Unique ID of a function. Uses reference equality.
    * Different functions can have the same name, but will have distinct IDs.
    */
  sealed trait FuncId extends Comparable[FuncId] {
    val sourceName: String

    override def compareTo(o: FuncId): Int = FuncIdOrdering.compare(this, o)
  }

  sealed trait EmittableFuncId extends FuncId

  sealed trait PositionedFuncId extends FuncId {
    val filePos: FilePos
  }

  /** ID of a function that was written in a source file. */
  class UserFuncId(override val sourceName: String, override val filePos: FilePos) extends EmittableFuncId with PositionedFuncId {

    override def toString: String = s"UserFuncId($sourceName)"
  }

  object UserFuncId {
    def unapply(id: UserFuncId): Some[(String, FilePos)] = Some(id.sourceName, id.filePos)
  }

  /** ID of a monomorphized function. It's an instantiation of another function. */
  case class FuncInstantiationId(originalFunc: FuncId, instStackTy: StackType) extends EmittableFuncId {
    override val sourceName: String = originalFunc.sourceName

    def actualIdentifier: String = originalFunc.sourceName + "$" + instStackTy

  }

  /** ID of a builtin function. This one uses name equality. */
  case class BuiltinFuncId(override val sourceName: String) extends EmittableFuncId {
    def mlirName: String = s"sigi::$sourceName"
  }

  /** ID of a variable declared in a local scope on the stack. */
  class StackValueId(override val sourceName: String,
                     val isFunction: Boolean,
                     override val filePos: FilePos) extends PositionedFuncId {
    def sourceRepr: String = if isFunction then s"\\$sourceName" else sourceName

    override def toString: String = sourceRepr + " at " + filePos
  }

  object StackValueId {
    def unapply(a: StackValueId): Option[(String, Boolean, FilePos)] = a match
      case s: StackValueId => Some((s.sourceName, s.isFunction, s.filePos))
      case _ => None
  }

  object FuncId {
    def unapply(id: FuncId): Option[(String, Option[FilePos])] = id match
      case id: PositionedFuncId => Some((id.sourceName, Some(id.filePos)))
      case id => Some((id.sourceName, None))
  }

  sealed trait KStatement extends KNode

  case class KBlock(stmts: List[KStatement]) extends KStatement

  case class KFunDef(id: UserFuncId, astTy: Option[AFunType], body: KExpr) extends KStatement

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
      case NameTopN(ids) => ids.map(_.sourceRepr).mkString("-> ", ", ", ";")
      case PushList(items) => items.mkString("[", ", ", "]")
      case Quote(FunApply(name)) => if Character.isAlphabetic(name(0)) then s"\\$name" else s"(\\$name)"
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

  case class VarNameDecl(isFunction: Boolean, name: String) extends Positional

  case class NameTopN(names: List[StackValueId]) extends KExpr


  class SigiParser(private val fileName: String = "") extends Parsers with PackratParsers {
    override type Elem = KToken

    def nextFunId(id: ID): UserFuncId = new UserFuncId(id.name, FilePos(id.pos, fileName))

    def toStackId(id: VarNameDecl): StackValueId =
      new StackValueId(id.name, id.isFunction, FilePos(id.pos, fileName))


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

    def funDef: Parser[KFunDef] =
      DEFINE_FUNC ~> id ~ (COLON ~> funTy).? ~ (OP("=") ~> expr) <~ PHAT_SEMI ^^ {
        case id ~ ty ~ body => KFunDef(nextFunId(id), ty, body).setPos(id.pos)
      }

    private def parexpr = LPAREN ~> expr <~ RPAREN

    private def opAsFunApply = accept("operator", { case op: OP => FunApply(op.opName).setPos(op.pos) })

    private def id: Parser[ID] = accept("identifier", { case id: ID => id })

    private def identAsFunApply = id ^^ { id => FunApply(id.name).setPos(id.pos) }

    private def ifelse: Parser[KExpr] =
      (IF() ~ parexpr.? ~ (expr ^^ Quote.apply)
        ~ rep(ELIF() ~ parexpr ~ (expr ^^ Quote.apply))
        ~ (ELSE ~> expr ^^ Quote.apply)
        ) ^^ {
        case (ifTok: IF) ~ (cond: Option[KExpr]) ~ (thenThunk: Quote) ~ (elifs: List[ELIF ~ KExpr ~ Quote]) ~ (elseThunk: Quote) =>
          def makeIf(thenThunk: Quote, elseThunk: Quote, position: Position): KExpr = {
            val ifFunction = Chain(FunApply("cond").setPos(position), FunApply("apply").setPos(position))
            Chain(Chain(thenThunk, elseThunk), ifFunction)
          }

          val foldedElseIf = elifs.foldRight[Quote](elseThunk) {
            case (elif ~ c ~ t, f) => Quote(Chain(c, makeIf(t, f, elif.pos)))
          }
          val ifelse = makeIf(thenThunk, foldedElseIf, ifTok.pos)

          cond.map(c => Chain(c, ifelse)).getOrElse(ifelse)
      }

    private def primary: Parser[KExpr] =
      positioned(
        TRUE ^^^ PushPrim(types.KBool, true)
          | FALSE ^^^ PushPrim(types.KBool, false)
          | identAsFunApply
          | accept("number", { case n@NUMBER(v) => PushPrim(types.KInt, v) })
          | accept("string", { case s@STRING(v) => PushPrim(types.KString, v) })
          | BACKSLASH ~> (opAsFunApply | identAsFunApply) ^^ Quote.apply
          | (LBRACKET ~> repsep(expr, COMMA) <~ RBRACKET ^^ PushList.apply)
          | (LPAREN ~> (expr | opAsFunApply) <~ RPAREN)
          | thunk
          | ifelse
        )

    private def varNameDecl: Parser[VarNameDecl] =
      BACKSLASH.? ~ id ^^ {
        case backslash ~ id => VarNameDecl(isFunction = backslash.isDefined, id.name).setPos(id.pos)
      }

    private def nameTopN: Parser[KExpr] =
      ARROW ~ rep1sep(varNameDecl, COMMA) <~ SEMI ^^ {
        case arrow ~ decls => NameTopN(decls.map(toStackId)).setPos(arrow.pos)
      }

    private def unary: Parser[KExpr] =
      (OP("-") | OP("+") | OP("~") | OP("!")).? ~ primary ^^ {
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
      case (funs: List[KFunDef]) ~ (expr: KExpr) => KFile(fileName, funs, expr)
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
        if names.map(_.sourceName).distinct.lengthCompare(names) != 0 then
          Some(SigiParseError.namesShouldBeUnique(node))
        else
          None
      case _ => None


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

    def parseExpr(code: String): Either[SigiCompilationError, KExpr] = {
      this (code, expr).flatMap(e => validate(e).toLeft(e))
    }

    def parseFunDef(code: String): Either[SigiCompilationError, KFunDef] = {
      this (code, funDef)
    }

    def parseFunType(code: String): Either[SigiCompilationError, AFunType] = {
      this (code, funTy)
    }

    def parseAndResolveFunType(scope: TypingScope)(code: String): Either[SigiCompilationError, StackType] = {
      parseFunType(code).flatMap(types.resolveFunType(scope)).map(_.stackTy)
    }

    def parseStmt(code: String): Either[SigiCompilationError, KStatement] = {
      this (code, statementList).flatMap(e => validate(e).toLeft(e))
    }

    def parseFile(fileContents: String): Either[SigiCompilationError, KFile] = {
      this (fileContents, file).flatMap(file => {
        val errors = validate(file)
        if errors.isEmpty
        then Right(file)
        else Left(SigiCompilationError.allOf(errors))
      })
    }
  }
}
