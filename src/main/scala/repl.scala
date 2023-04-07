package de.cfaed.sigi


package repl {

  import ast.*
  import datamodel.{TypeDescriptor, TypeParm}
  import types.*
  import types.StackType.canonicalize

  import de.cfaed.sigi.builtins
  import de.cfaed.sigi.builtins.BuiltinFunSpec

  import scala.annotation.tailrec


  @main
  def repl(): Unit = {
    val scanner = scala.io.StdIn

    def showSomethingNice(before: Env, after: Env)(t: TypedStmt): Unit = t match
      case TExprStmt(TChain(_, TEvalBarrier(_), term)) => showSomethingNice(before, after)(TExprStmt(term))
      case TExprStmt(e) =>
        val consumed = before.stack.take(e.stackTy.consumes.length).reverseIterator.mkString(", ")
        val produced = after.stack.take(e.stackTy.produces.length).reverseIterator.mkString(", ")
        println(e.stackTy)
        println(s"$consumed -> $produced".trim)

      case TFunDef(id, ty, _) => println(s"Defined function ${id.sourceName}: $ty")
      case TBlock(st) =>
        // this does not really work as the environments are different between each statement.
        // but at the same time the repl never parses a block so it does not matter.
        st.foreach(showSomethingNice(before, after))


    def chainWithLastExpr(typedStmt: KStatement, lastExpr: Option[TypedExpr]): KStatement = {
      (typedStmt, lastExpr) match
        case (KExprStatement(expr), Some(last)) => KExprStatement(Chain(OpaqueExpr(last), expr))
        case _ => typedStmt
    }

    @tailrec
    def doRepl(line: String, env: Env, lastExpr: Option[TypedExpr]): Unit = {
      if (line == "exit" || line == ":wq" || line == ":q" || line == "/exit") return

      val result: Either[SigiError, (Env, TypedStmt)] = for {
        parsed <- new SigiParser(fileName = "repl").parseStmt(line)
        chained = chainWithLastExpr(parsed, lastExpr)
        typed <- doValidation(env.toTypingScope)(chained)
        env2 <- eval(typed)(env)
      } yield {
        showSomethingNice(env, env2)(typed)
        (env2, typed)
      }

      val (newEnv, newLastExpr) = result match
        case Right((env2, TExprStmt(te))) => (env2, Some(te))
        case Right((env2, _)) => (env2, lastExpr)
        case Left(error) =>
          println(error)
          (env, lastExpr)

      print("> ")
      doRepl(scanner.readLine(), newEnv, newLastExpr)
    }

    print("> ")
    doRepl(scanner.readLine(), env = Env.DefaultReplEnv, lastExpr = None)
  }



  type EvalResult = Either[SigiEvalError, Env]

  sealed trait KValue {
    override def toString: String = this match
      case VPrimitive(types.KString, value) => s"\"$value\""
      case VPrimitive(_, value) => value.toString
      case VFun(name, t, _) => s"(${name.getOrElse("unnamed")} : $t)"
      case VList(_, items) => items.mkString("[", ", ", "]")

    def stackType: StackType = this match
      case VPrimitive(t, _) => StackType.pushOne(t)
      case VFun(_, st, _) => st
      case VList(t, _) => StackType.pushOne(t)

    def dataType: KDataType = this match
      case VPrimitive(t, _) => t
      case VFun(_, st, _) => KFun(st)
      case VList(t, _) => t

  }
  case class VPrimitive[T](ty: KPrimitive[T], v: T) extends KValue
  object VBool {
    def apply(boolean: Boolean): VPrimitive[Boolean] = VPrimitive(KBool, boolean)

    def unapply(prim: VPrimitive[_]): Option[Boolean] = prim match
      case VPrimitive(KBool, v) => Some(v)
      case _ => None
  }
  object VNum {
    def apply(i: Int): VPrimitive[Int] = VPrimitive(KInt, i)
    def unapply(prim: VPrimitive[_]): Option[Int] = prim match
      case VPrimitive(KInt, v) => Some(v)
      case _ => None
  }

  case class VFun(name: Option[String], t: StackType, definition: Env => EvalResult) extends KValue
  case class VList(ty: types.KList, items: List[KValue]) extends KValue


  case class Env(vars: Map[FuncId, KValue],
                 stack: List[KValue],
                 typesInScope: Map[String, datamodel.TypeDescriptor]) {

    def apply(id: FuncId): Either[SigiEvalError, KValue] = vars.get(id).toRight(SigiEvalError.undef(id.sourceName))

    def push(v: KValue): Env = Env(vars, v :: stack, typesInScope)

    def stackToString: String = stack.reverse.mkString("[", ", ", "]")

    def varsToString: String = (vars -- Env.Default.vars.keys).map { case (k, v) => s"${k.sourceName}: $v" }.mkString("{", ", ", "}")

    private def bindingTypes: types.BindingTypes = vars.map((k: FuncId, v) => (k.sourceName, VarBinding(k, v.dataType)))

    export typesInScope.get as getType

    def toTypingScope: TypingScope = TypingScope(bindingTypes, typesInScope)
  }

  object Env {
    def defaultEnv(builtins: IterableOnce[BuiltinFunSpec]) =
      new Env(
        builtins.map(spec => spec.id -> spec.asValue).toMap,
        Nil,
        TypeDescriptor.Predefined
        )

    val Default: Env = defaultEnv(builtins.BuiltinSpecs)
    val DefaultReplEnv: Env = defaultEnv(builtins.BuiltinSpecs ++ builtins.ReplBuiltinSpecs)
    ,
  }

  def applyValue(env: Env)(value: KValue): EvalResult =
    value match
      case value@VPrimitive(_, _) => Right(env.push(value))
      case value@VList(_, _) => Right(env.push(value))
      case VFun(_, _, definition) =>
        // the evaluation performs its own type checking
        definition(env)
          // Remove bindings created by the function call from the environment.
          // Just keep the stack.
          // todo scoping:
          //  what is a block in the source,
          //  should we use that as the scope delimiter,
          //  should functions use one as body
          .map(e => env.copy(stack = e.stack))

  def eval(stmt: TypedStmt)(env: Env): EvalResult = stmt match
    case TBlock(stmts) =>
      stmts.foldLeft[EvalResult](Right(env)) { (env, newStmt) =>
        env.flatMap(eval(newStmt))
      }
    case TFunDef(id, ty, body) =>
      Right(
        env.copy(vars = env.vars.updated(id, VFun(Some(id.sourceName), ty, eval(body))))
      )
    case TExprStmt(e) => eval(e)(env)

  def eval(knode: types.TypedExpr)(env: Env): EvalResult = knode match
    case TPushPrim(ty, value) => Right(env.push(VPrimitive(ty, value)))
    case TPushList(ty, items) =>
      val stackLen = env.stack.length
      eval(TBlock(items.map(TExprStmt.apply)))(env).flatMap(env => {
        val newItems = env.stack.length - stackLen
        if newItems != items.length then
          Left(SigiEvalError(s"Problem building list, expected ${items.length} new items on stack, got $newItems. This should have been caught by the type checker."))
        else
          val (listItems, stackRest) = env.stack.splitAt(newItems)
          Right(env.copy(stack = VList(ty, listItems.reverse) :: stackRest))
      })
    case TFunApply(_, id) => env(id).flatMap(applyValue(env))
    case TChain(_, a, b) => eval(a)(env).flatMap(e2 => eval(b)(e2))
    case TEvalBarrier(_) => Right(env) // do nothing, already evaluated
    case TPushQuote(e) => Right(env.push(VFun(Some("(quote)"), e.stackTy, eval(e))))
    case node@TNameTopN(_, names) =>
      val (topOfStack, stackTail) = env.stack.splitAt(names.length)
      if topOfStack.lengthCompare(names.length) != 0 then
        Left(SigiEvalError.stackTypeError(node.stackTy, env))
      else
        Right(env.copy(
          vars = env.vars ++ names.zip(topOfStack.reverseIterator),
          stack = stackTail))
}
