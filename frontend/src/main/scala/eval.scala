package de.cfaed.kitten


import java.util.Scanner

package eval {

  import ast.*
  import types.{BinOpType, StackType}

  import scala.annotation.tailrec


  @main
  def repl(): Unit = {
    val scanner = Scanner(System.in)

    @tailrec
    def doRepl(line: String, env: Env): Unit = {
      if (line == "exit") return

      val result: Either[KittenError, Env] = for {
        parsed <- ast.parse(line)
        t <- types.computeType(parsed.ast)
        //println(t)
        env2 <- eval(env)(parsed.ast)
      } yield {
        println(s"  ${parsed.ast}    : $t")
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
      case VNum(num) => num.toString
      case VFun(name, t, _) => s"($name : $t)"
  }
  case class VNum(num: Int) extends KValue
  case class VFun(name: Option[String], t: StackType, definition: Env => EvalResult) extends KValue


  case class Env(vars: Map[String, KValue],
                 stack: List[KValue]) {
    def apply(name: String): Either[KittenEvalError, KValue] = vars.get(name).toRight(KittenEvalError.undef(name))

    def push(v: KValue): Env = Env(vars, v :: stack)

    def stackToString: String = stack.mkString("[", " :: ", "]")
  }

  object Env {
    private def binOp(name: String, definition: (Int, Int) => Int): (String, VFun) = {
      (name, VFun(Some(name), types.BinOpType, env => {
        env.stack match
          case VNum(a) :: VNum(b) :: tail =>
            try {
              val res = definition(a, b)
              Right(env.copy(stack = VNum(res) :: tail))
            } catch {
              case e: Exception => Left(KittenEvalError(s"Error executing op $name: ${e.getMessage}"))
            }
          case _ =>
            Left(KittenEvalError.stackTypeError(types.BinOpType, env))
      }))
    }
    private val predefinedSymbols: Map[String, KValue] = Map(
      binOp("+", _ + _),
      binOp("*", _ * _)
    )
    val default: Env = Env(predefinedSymbols, Nil)
  }

  def applyValue(env: Env)(value: KValue): EvalResult =
    value match
      case num@VNum(_) => Right(env.push(num))
      case VFun(name, t, definition) =>
        if (env.stack.lengthCompare(t.consumes.length) < 0)
          Left(KittenEvalError.stackTypeError(t, env))
        else
          definition(env)


  def eval(env: Env)(knode: KExpr): EvalResult = {
    knode match
      case Number(value) => Right(env.push(VNum(value)))
      case FunApply(name) => env(name).flatMap(applyValue(env))
      case Chain(a, b) => eval(env)(a).flatMap(e2 => eval(e2)(b))
  }
}
