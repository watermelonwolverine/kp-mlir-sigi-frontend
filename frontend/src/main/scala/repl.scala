package de.cfaed.sigi


package repl {

  import ast.*
  import datamodel.{TypeDescriptor, TypeParm}
  import types.*
  import types.StackType.canonicalize

  import scala.annotation.tailrec


  @main
  def repl(): Unit = {
    val scanner = scala.io.StdIn

    def showSomethingNice(before: Env, after: Env)(t: TypedStmt): Unit = t match
      case TExprStmt(e) =>
      // val consumed = before.stack.take(e.stackTy.consumes.length).reverseIterator.mkString(", ")
      // val produced = after.stack.take(e.stackTy.produces.length).reverseIterator.mkString(", ")
      // println(s"$consumed -> $produced")

      case TFunDef(name, ty, _) => println(s"Defined function $name: $ty")
      case TBlock(st) => st.foreach(showSomethingNice(before, after)) // todo this does not work! the environments are different

    @tailrec
    def doRepl(line: String, env: Env): Unit = {
      if (line == "exit") return

      val result: Either[SigiError, Env] = for {
        parsed <- SigiParser.parseStmt(line)
        typed <- doValidation(env.toTypingScope)(parsed)
        env2 <- eval(typed)(env)
      } yield {
        showSomethingNice(env, env2)(typed)
        env2
      }

      val newEnv: Env = result match
        case Right(e) => e
        case Left(error) =>
          println(error)
          env

      print("> ")
      doRepl(scanner.readLine(), newEnv)
    }

    print("> ")
    doRepl(scanner.readLine(), Env.Default.copy(vars = Env.Default.vars ++ builtins.ReplBuiltins))
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
  }
  object VNum {
    def apply(i: Int): VPrimitive[Int] = VPrimitive(KInt, i)
    def unapply(prim: VPrimitive[_]): Option[Int] = prim match
      case VPrimitive(KInt, v) => Some(v)
      case _ => None
  }

  case class VFun(name: Option[String], t: StackType, definition: Env => EvalResult) extends KValue
  case class VList(ty: types.KList, items: List[KValue]) extends KValue


  case class Env(vars: Map[String, KValue],
                 stack: List[KValue],
                 typesInScope: Map[String, datamodel.TypeDescriptor]) {
    def apply(name: String): Either[SigiEvalError, KValue] = vars.get(name).toRight(SigiEvalError.undef(name))

    def push(v: KValue): Env = Env(vars, v :: stack, typesInScope)

    def stackToString: String = stack.reverse.mkString("[", ", ", "]")
    def varsToString: String = (vars -- Env.Default.vars.keys).map { case (k, v) => s"$k: $v" }.mkString("{", ", ", "}")
    def bindingTypes: types.BindingTypes = vars.map((k, v) => (k, v.dataType))

    export typesInScope.get as getType
    
    def toTypingScope: TypingScope = TypingScope(bindingTypes, typesInScope)
  }

  object Env {
    val Default: Env = Env(builtins.PredefinedSymbols, Nil, TypeDescriptor.Predefined)
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
    case TFunDef(name, ty, body) =>
      Right(
        env.copy(vars = env.vars.updated(name, VFun(Some(name), ty, eval(body))))
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
    case TFunApply(_, name) => env(name).flatMap(applyValue(env))
    case TChain(_, a, b) => eval(a)(env).flatMap(e2 => eval(b)(e2))
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