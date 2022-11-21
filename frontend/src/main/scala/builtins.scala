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

  val ReplBuiltins: Map[String, KValue] = Map(
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
  )

  val PredefinedSymbols: Map[String, KValue] = Map(
    // These operators need to be there because the parser refers to them.
    // Those bindings cannot be shadowed because they are not valid identifiers.
    binOp("+", _ + _),
    binOp("-", _ - _),
    binOp("*", _ * _),
    binOp("/", _ / _),
    binOp("%", _ % _),
    unaryOp("unary-", a => -a),
    unaryOp("unary+", a => a),
    unaryOp("unary~", a => a ^ a),

    // These are core function. Also see list of cat builtins: https://github.com/cdiggins/cat-language
    // TODO apply   : (('S -> 'R) 'S -> 'R)
    //      compose : (('B -> 'C) ('A -> 'B) 'S -> ('A -> 'C) 'S)
    //      while   : (('S -> Bool 'R) ('R -> 'S) 'S -> 'S)
    //  need row type variables
    stackFun("pop", StackType.generic1(tv => StackType(consumes = List(tv))), {
      case _ :: tl => Right(tl)
    }),
    // duplicate top of the stack
    stackFun("dup", StackType.generic1(tv => StackType(consumes = List(tv), produces = List(tv, tv))), {
      case hd :: tl => Right(hd :: hd :: tl)
    }),
    // swap top elements
    stackFun("swap", StackType.generic2((ta, tb) => StackType(consumes = List(ta, tb), produces = List(tb, ta))), {
      case a :: b :: tl => Right(b :: a :: tl)
    }),
    // quote top of the stack
    stackFun("quote", StackType.generic1(ta => StackType(consumes = List(ta), produces = List(KFun(StackType.pushOne(ta))))), {
      case a :: tl => Right(VFun(Some("quote"), StackType.generic1(StackType.pushOne), env => Right(env.push(a))) :: tl)
    }),
    // select one of two values
    stackFun("cond", StackType.generic1(ta => StackType(consumes = List(KBool, ta, ta), produces = List(ta))), {
      case elseV :: thenV :: VPrimitive(KBool, condition) :: tl =>
        Right((if condition then thenV else elseV) :: tl)
    }),

    // if:   bool, (-> 'a), (-> 'a) -> 'a
    // todo this is basically `cond apply` in terms of Cat builtins
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
