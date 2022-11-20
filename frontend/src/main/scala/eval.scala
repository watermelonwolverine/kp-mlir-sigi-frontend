package de.cfaed.kitten


import java.util.Scanner

package eval {

  import de.cfaed.kitten.ast as parser
  import ast.*
  import types.{BinOpType, KDataType, KFun, KInt, KPrimitive, StackType, TypingScope}
  import types.StackType.canonicalize

  import scala.annotation.tailrec


  @main
  def repl(): Unit = {
    val scanner = Scanner(System.in)

    @tailrec
    def doRepl(line: String, env: Env): Unit = {
      if (line == "exit") return

      val result: Either[KittenError, Env] = for {
        parsed <- parser.parse(line)
        t <- types.computeType(env.toSymbolic)(parsed.ast)
        //println(t)
        env2 <- eval(parsed.ast)(env)
      } yield {
        println(s"  ${parsed.ast}    : ${canonicalize(t._1)}")
        println(s"  ${env2.stackToString}")
        env2
      }

      val newEnv: Env = result match
        case Right(e) => e
        case Left(error) =>
          println(error)
          env

      print("> ")
      doRepl(scanner.nextLine(), newEnv)
    }

    print("> ")
    doRepl(scanner.nextLine(), Env.default)
  }

  type EvalResult = Either[KittenEvalError, Env]

  sealed trait KValue {
    override def toString: String = this match
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
  object VNum {
    def apply(i: Int): VPrimitive[Int] = VPrimitive(KInt, i)
    def unapply(prim: VPrimitive[_]): Option[Int] = prim match
      case VPrimitive(KInt, v) => Some(v)
      case _ => None
  }

  case class VFun(name: Option[String], t: StackType, definition: Env => EvalResult) extends KValue
  case class VList(t: types.KList, items: List[KValue]) extends KValue


  case class Env(vars: Map[String, KValue],
                 stack: List[KValue]) {
    def apply(name: String): Either[KittenEvalError, KValue] = vars.get(name).toRight(KittenEvalError.undef(name))

    def push(v: KValue): Env = Env(vars, v :: stack)

    def stackToString: String = stack.mkString("[", " :: ", "]")
    def varsToString: String = (vars -- Env.default.vars.keys).map { case (k, v) => s"${k}: $v" }.mkString("{", ", ", "}")
    def toSymbolic: TypingScope = {
      TypingScope(
        bindings = vars.map((k, v) => (k, v.stackType)),
      )
    }
  }

  object Env {
    private def binOp(name: String, definition: (Int, Int) => Int): (String, VFun) = {
      fun(name, types.BinOpType, t => env => {
        env.stack match
          case VNum(a) :: VNum(b) :: tail =>
            try {
              val res = definition(a, b)
              Right(env.copy(stack = VNum(res) :: tail))
            } catch {
              case e: Exception => Left(KittenEvalError(s"Error executing op $name: ${e.getMessage}"))
            }
          case _ => Left(KittenEvalError.stackTypeError(t, env))
      })
    }

    private def unaryOp(name: String, definition: Int => Int): (String, VFun) = {
      fun(name, types.UnaryOpType, t => env => {
        env.stack match
          case VNum(a) :: tail =>
            try {
              val res = definition(a)
              Right(env.copy(stack = VNum(res) :: tail))
            } catch {
              case e: Exception => Left(KittenEvalError(s"Error executing op $name: ${e.getMessage}"))
            }
          case _ => Left(KittenEvalError.stackTypeError(t, env))
      })
    }

    private def fun(name: String, stackType: StackType, definition: StackType => Env => EvalResult): (String, VFun) = {
      (name, VFun(Some(name), stackType, definition(stackType)))
    }
    private def stackFun(name: String, stackType: StackType, definition: PartialFunction[List[KValue], Either[KittenEvalError, List[KValue]]]): (String, VFun) = {
      (name, VFun(Some(name), stackType, env => {
        definition.applyOrElse(env.stack, _ => Left(KittenEvalError.stackTypeError(stackType, env)))
          .map(e => env.copy(stack = e))
      }))
    }

    // intrinsics should have a name which cannot be an identifier in the source lang
    val Intrinsic_if = ":if:"

    private val predefinedSymbols: Map[String, KValue] = Map(
      binOp("+", _ + _),
      binOp("-", _ - _),
      binOp("*", _ * _),
      binOp("/", _ / _),
      binOp("%", _ % _),
      unaryOp("unary-", a => -a),
      unaryOp("unary+", a => a),
      unaryOp("unary~", a => a ^ a),
      stackFun("pop", StackType.generic1(tv => StackType(consumes = List(tv))), {
        case _ :: tl => Right(tl)
      }),
      // duplicate top of the stack
      stackFun("dup", StackType.generic1(tv => StackType(consumes = List(tv), produces = List(tv, tv))), {
        case hd :: tl => Right(hd :: hd :: tl)
      }),
      // consume top of the stack and print it
      // this is `pp pop`
      stackFun("show", StackType.generic1(tv => StackType(consumes = List(tv))), {
        case hd :: tl =>
          println(s"show: $hd")
          Right(tl)
      }),
      // print and pass: print the top of the stack but leave it there
      // this function is equivalent to `dup show`
      stackFun("pp", StackType.generic1(StackType.symmetric1), {
        case stack@(hd :: _) =>
          println(s"pp: $hd")
          Right(stack)
      }),
      fun("env", StackType(), _ => env => {
        println(s"stack: ${env.stackToString}")
        println(s"env: ${env.varsToString}")
        Right(env)
      }),
      stackFun("typeof", StackType.generic1(StackType.symmetric1), {
        case stack@(hd :: _) =>
          println("typeof: " + hd.dataType)
          Right(stack)
      }),
      // if:   (-> 'a), (-> 'a), bool -> 'a
      fun(Intrinsic_if, StackType.generic1(tv => {
        val thunkT = KFun(StackType.pushOne(tv))
        StackType(
          consumes = List(thunkT, thunkT, types.KBool),
          produces = List(tv)
        )
      }), t => env => {
        env.stack match
          case VFun(_, _, thenDef) :: VFun(_, _, elseDef) :: VPrimitive(types.KBool, b) :: tl =>
            val newEnv = env.copy(stack = tl)
            if (b) thenDef(newEnv) else elseDef(newEnv)
          case _ => Left(KittenEvalError.stackTypeError(t, env))
      }),
    )
    val default: Env = Env(predefinedSymbols, Nil)
  }

  def applyValue(env: Env)(value: KValue): EvalResult =
    value match
      case value@VPrimitive(_, _) => Right(env.push(value))
      case value@VList(_, _) => Right(env.push(value))
      case VFun(_, t, definition) =>
        if (env.stack.lengthCompare(t.consumes.length) < 0)
          Left(KittenEvalError.stackTypeError(t, env))
        else
          definition(env)


  def eval(knode: KExpr)(env: Env): EvalResult = {
    knode match
      case PushPrim(ty, value) => Right(env.push(VPrimitive(ty, value)))
      case PushList(items) =>
        val stackLen = env.stack.length
        items.foldLeft[EvalResult](Right(env)) { (env, item) =>
          env.flatMap(e => eval(item)(e))
        }.flatMap(env => {
          val newItems = env.stack.length - stackLen 
          if newItems != items.length then
            Left(KittenEvalError(s"Problem building list, expected ${items.length} new items on stack, got $newItems. This should have been caught by the type checker."))
          else
            val (listItems, stackRest) = env.stack.splitAt(newItems)
            Right(env.copy(stack =VList(ty, listItems) stackRest))
            
        })
      case FunApply(name) => env(name).flatMap(applyValue(env))
      case Chain(a, b) => eval(a)(env).flatMap(e2 => eval(b)(e2))
      case q@Quote(e) => Right(env.push(VFun(Some(q.toString), types.computeType(env.toSymbolic)(e).map(_._1).right.get, eval(e))))
      case node@NameTopN(names) =>
        val (topOfStack, stackTail) = env.stack.splitAt(names.length)
        if topOfStack.lengthCompare(names.length) != 0 then
          Left(KittenEvalError.stackTypeError(node.stackType, env))
        else
          Right(env.copy(
            vars = env.vars ++ names.zip(topOfStack.reverseIterator),
            stack = stackTail))

  }
}
