package de.cfaed.sigi

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
package builtins {

  import ast.*
  import types.*
  import repl.*

  import scala.collection.immutable.List


  type ReplEvalFunction = Env => EvalResult

  sealed class BuiltinCompilationStrategy

  /** This builtin is eliminated in the frontend by [[emitmlir]]. */
  case object FrontendIntrinsic extends BuiltinCompilationStrategy

  case class StdLibDefinition(
    definition: TFunDef
  ) extends BuiltinCompilationStrategy

  case class MlirDefinition(
    definition: String => String
  ) extends BuiltinCompilationStrategy


  case class BuiltinFunSpec(
    surfaceName: String,
    stackType: StackType,
    evaluationStrategy: ReplEvalFunction,
    compilationStrategy: BuiltinCompilationStrategy
  ) {
    val id: BuiltinFuncId = BuiltinFuncId(surfaceName)

    def asValue: VFun = VFun(Some(surfaceName), stackType, evaluationStrategy)
  }

  private def genericCmpOp(name: String, negated: Boolean): BuiltinFunSpec = {
    stackFun(name, StackType.generic1(tv => StackType(consumes = List(tv, tv), produces = List(KBool)))) {
      case b :: a :: tail =>
        val eq = a == b
        Right(VBool(eq != negated) :: tail)
    }
  }

  private def cmpOp(name: String, definition: (Int, Int) => Boolean): BuiltinFunSpec = {
    stackFun(name, StackType(consumes = List(KInt, KInt), produces = List(KBool))) {
      case VNum(b) :: VNum(a) :: tail =>
        val res = definition(a, b)
        Right(VBool(res) :: tail)
    }
  }


  private def boolUnaryOp(name: String, definition: (Boolean) => Boolean): BuiltinFunSpec = {
    stackFun(name, types.unaryOpType(KBool)) {
      case VBool(a) :: tail =>
        val res = definition(a)
        Right(VBool(res) :: tail)
    }
  }
  private def boolOp(name: String, definition: (Boolean, Boolean) => Boolean): BuiltinFunSpec = {
    stackFun(name, types.binOpType(KBool)) {
      case VBool(b) :: VBool(a) :: tail =>
        val res = definition(a, b)
        Right(VBool(res) :: tail)
    }
  }

  private def binOp(name: String, definition: (Int, Int) => Int): BuiltinFunSpec = {
    stackFun(name, types.binOpType(KInt)) {
      case VNum(b) :: VNum(a) :: tail =>
        val res = definition(a, b)
        Right(VNum(res) :: tail)
    }
  }

  private def unaryOp(name: String, definition: Int => Int): BuiltinFunSpec = {
    stackFun(name, types.unaryOpType(KInt)) {
      case VNum(a) :: tail =>
        val res = definition(a)
        Right(VNum(res) :: tail)
    }
  }

  private def fun(name: String, stackType: StackType, comp: BuiltinCompilationStrategy = FrontendIntrinsic)
                 (definition: StackType => Env => EvalResult): BuiltinFunSpec = {
    BuiltinFunSpec(
      surfaceName = name,
      stackType = stackType,
      evaluationStrategy = definition(stackType),
      compilationStrategy = comp
      )
  }

  private def stackFun(name: String, stackType: StackType, compilationStrategy: BuiltinCompilationStrategy = FrontendIntrinsic)
                      (definition: PartialFunction[List[KValue], Either[SigiEvalError, List[KValue]]]): BuiltinFunSpec = {
    fun(name, stackType, compilationStrategy) { ty =>
      env =>
        definition.applyOrElse(env.stack, _ => Left(SigiEvalError.stackTypeError(ty, env)))
          .map(e => env.copy(stack = e))
    }
  }

  val ReplBuiltinSpecs: Set[BuiltinFunSpec] =
    Set(
      fun("env", StackType()) { _ =>
        env =>
          println(s"stack (top is right): ${env.stackToString}")
          println(s"env: ${env.varsToString}")
          Right(env)
      },
      stackFun("typeof", StackType.generic1(StackType.symmetric1)) {
        case stack@(hd :: _) =>
          println("typeof: " + hd.dataType)
          Right(stack)
      }
      )

  val BuiltinSpecs: Set[BuiltinFunSpec] = {
    // this is a forward declaration, the dialect should do something with it.
    val mlirFwdDeclaration = MlirDefinition(name => s"func.func private @\"$name\"(!sigi.stack) -> !sigi.stack")

    val cond = // select one of two values
      stackFun("cond", StackType.generic1(ta => StackType(consumes = List(KBool, ta, ta), produces = List(ta))),
               compilationStrategy = FrontendIntrinsic) {
        case elseV :: thenV :: VPrimitive(KBool, condition) :: tl =>
          Right((if condition then thenV else elseV) :: tl)
      }

    val pop = stackFun("pop", StackType.generic1(tv => StackType(consumes = List(tv)))) {
      case _ :: tl => Right(tl)
    }

    val dup = stackFun("dup", StackType.generic1(tv => StackType(consumes = List(tv), produces = List(tv, tv)))) {
      case hd :: tl => Right(hd :: hd :: tl)
    }
    // print and pass: print the top of the stack but leave it there
    val pp = stackFun("pp", StackType.generic1(StackType.symmetric1),
                      compilationStrategy = mlirFwdDeclaration) {
      case stack@(hd :: _) =>
        println(s"pp: $hd")
        Right(stack)
    }
    val apply = fun("apply", {
      // apply   : ('S ('S -> 'R) -> 'R)
      val row = KRowVar.rowVarGenerator()
      val S = row()
      val R = row()
      StackType(consumes = List(S, KFun(StackType(List(S), List(R)))), produces = List(R))
    }) { t =>
      env =>
        env.stack match
          // here we assume the term is well-typed, and so the fun is compatible with the rest of the stack.
          case VFun(_, _, fundef) :: rest => fundef(env.copy(stack = rest))
          case _ => Left(SigiEvalError.stackTypeError(t, env))
    }

    Set(
      // These operators need to be there because the parser refers to them.
      // Those bindings cannot be shadowed because they are not valid identifiers.
      binOp("+", _ + _),
      binOp("-", _ - _),
      binOp("*", _ * _),
      binOp("/", _ / _),
      binOp("%", _ % _),
      cmpOp("<", _ < _),
      cmpOp(">", _ > _),
      cmpOp(">=", _ >= _),
      cmpOp("<=", _ <= _),
      genericCmpOp("=", false),
      genericCmpOp("<>", true),
      unaryOp("unary_-", a => -a),
      unaryOp("unary_+", a => a),
      unaryOp("unary_~", a => ~a),

      boolOp("and", _ & _),
      boolOp("or", _ | _),
      boolOp("xor", _ ^ _),
      boolUnaryOp("not", !_),
      boolUnaryOp("unary_!", !_),

      // These are core function. Also see list of cat builtins: https://github.com/cdiggins/cat-language
      // TODO compose : ('S ('B -> 'C) ('A -> 'B) -> 'S ('A -> 'C))
      //      while   : ('S ('S -> 'R bool) ('R -> 'S) -> 'S)
      apply,
      pop,
      dup,
      cond,
      pp,
      // swap top elements
      stackFun("swap", StackType.generic2((ta, tb) => StackType(consumes = List(ta, tb), produces = List(tb, ta)))) {
        case a :: b :: tl => Right(b :: a :: tl)
      },
      // quote top of the stack
      stackFun("quote", StackType.generic1(ta => StackType(consumes = List(ta), produces = List(KFun(StackType.pushOne(ta)))))) {
        case a :: tl => Right(VFun(Some("quote"), StackType.generic1(StackType.pushOne), env => Right(env.push(a))) :: tl)
      },

      stackFun("show", StackType.generic1(a => StackType(consumes = List(a))),
               compilationStrategy = mlirFwdDeclaration) {
        case stack@(hd :: _) =>
          println(s"$hd")
          Right(stack)
      }
      )
  }

  val PredefinedSymbols: Map[String, KValue] = BuiltinSpecs.map(s => s.surfaceName -> s.asValue).toMap
  val ReplBuiltins: Map[String, KValue] = ReplBuiltinSpecs.map(s => s.surfaceName -> s.asValue).toMap

}
