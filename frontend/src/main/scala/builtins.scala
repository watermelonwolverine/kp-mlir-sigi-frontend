package de.cfaed.kitten

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
package builtins {

  import ast.*
  import types.*
  import eval.*


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

  val PredefinedSymbols: Map[String, KValue] = Map(
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
    // if:   bool, (-> 'a), (-> 'a) -> 'a
    fun(Intrinsic_if, StackType.generic1(tv => {
      val thunkT = KFun(StackType.pushOne(tv))
      StackType(
        consumes = List(types.KBool, thunkT, thunkT),
        produces = List(tv)
      )
    }), t => env => {
      env.stack match
        case VFun(_, _, elseDef) :: VFun(_, _, thenDef) :: VPrimitive(types.KBool, condition) :: tl =>
          val newEnv = env.copy(stack = tl)
          if (condition) thenDef(newEnv) else elseDef(newEnv)
        case _ => Left(KittenEvalError.stackTypeError(t, env))
    }),
  )
}
