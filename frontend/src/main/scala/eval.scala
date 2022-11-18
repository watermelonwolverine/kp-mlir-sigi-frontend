package de.cfaed.kitten


import java.util.Scanner

package eval {

  import ast.*

  import de.cfaed.kitten.types.StackType


  @main
  def repl(): Unit = {
    val scanner = Scanner(System.in)
    print("> ")
    var line = scanner.nextLine()
    while (line != "/exit") {
      ast.parse(line) match
        case Left(error) => println(s"Error $error")
        case Right(ParsedCode(_, ast)) => println(s"  $ast")
      print("> ")
      line = scanner.nextLine()
    }
  }


  sealed trait KValue
  case class VNum(num:Int)
  case class VFun(t: StackType, definition: Env => KValue)


  case class Env(vars: Map[String, KValue])

  def eval(knode: KExpr, env: Env): Either[KittenEvalError, (KValue, Env)] = {
    knode match
      case Number(value) => Right((value, env))
      case Var(name) => env.vars.get(name).toRight(KittenEvalError.undef(name), env)
      case Chain(a, b) => {
        
      }
  }
  case class KittenEvalError(msg: String)
  object KittenEvalError {
    def undef(name: String): KittenEvalError = KittenEvalError(s"Undefined name $name")
  }
}
