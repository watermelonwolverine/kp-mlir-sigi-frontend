package de.cfaed.sigi

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
package builtins {

  import ast.*
  import types.*
  import repl.*

  import de.cfaed.sigi.debug.NoopLogger

  import scala.collection.immutable.List


  type ReplEvalFunction = Env => EvalResult

  sealed trait CompilationStrategy

  /** This builtin is eliminated in the frontend by [[emitmlir]]. */
  case object FrontendIntrinsic extends CompilationStrategy

  case class CompileFunctionDefinition(
    definition: TFunDef
  ) extends CompilationStrategy

  case class EmitMlirDefinition(
    definition: String => String
  ) extends CompilationStrategy


  case class BuiltinFunSpec(
    surfaceName: String,
    stackType: StackType,
    evaluationStrategy: ReplEvalFunction,
    compilationStrategy: CompilationStrategy
  ) {
    val id: BuiltinFuncId = BuiltinFuncId(surfaceName)

    def asValue: VFun = VFun(Some(surfaceName), stackType, evaluationStrategy)

    def asVarBinding: VarBinding = VarBinding(id, KFun(stackType))
  }

  object BuiltinFuncIdWithSpec {
    /** Convenience extractor to get the spec in one go. */
    def unapply(id: BuiltinFuncId): (String, BuiltinFunSpec) =
      (id.sourceName,
        builtins.BuiltinSpecs.find(_.id == id).get)
  }

  object BuiltinWithCompilStrategy {
    /** Convenience extractor to get the spec in one go. */
    def unapply(id: BuiltinFuncId): Option[(String, CompilationStrategy)] = id match
      case BuiltinFuncIdWithSpec(name, BuiltinFunSpec(_, _, _, compil)) => Some(name, compil)
      case _ => None
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

  private def binOpStr(name: String, definition: (String, String) => String): BuiltinFunSpec = {
    stackFun(name, types.binOpType(KString)){
      case VString(b) :: VString(a) :: tail =>
        val res = definition(a,b)
        Right(VString(res) :: tail)
    }
  }

  private def unaryOp(name: String, definition: Int => Int): BuiltinFunSpec = {
    stackFun(name, types.unaryOpType(KInt)) {
      case VNum(a) :: tail =>
        val res = definition(a)
        Right(VNum(res) :: tail)
    }
  }

  private def unaryOpStr(name: String, definition: String => String): BuiltinFunSpec = {
    stackFun(name, types.unaryOpType(KString)) {
      case VString(a) :: tail =>
        val res = definition(a)
        Right(VString(res) :: tail)
    }
  }

  private def fun(name: String, stackType: StackType, comp: CompilationStrategy = FrontendIntrinsic)
                 (definition: StackType => Env => EvalResult): BuiltinFunSpec = {
    BuiltinFunSpec(
      surfaceName = name,
      stackType = stackType,
      evaluationStrategy = definition(stackType),
      compilationStrategy = comp
      )
  }

  private def stackFun(name: String, stackType: StackType, compilationStrategy: CompilationStrategy = FrontendIntrinsic)
                      (definition: PartialFunction[List[KValue], Either[SigiEvalError, List[KValue]]]): BuiltinFunSpec = {
    fun(name, stackType, compilationStrategy) { ty =>
      env =>
        definition.applyOrElse(env.stack, _ => Left(SigiEvalError.stackTypeError(ty, env)))
          .map(e => env.copy(stack = e))
    }
  }
  private def stdLibFun(funsInScope: BuiltinFunSpec*)(code: String): BuiltinFunSpec = {
    val typingScope = TypingScope(funsInScope.map(f => f.surfaceName -> f.asVarBinding).toMap, datamodel.TypeDescriptor.Predefined)(using NoopLogger)

    val result = for {
      tree <- new ast.SigiParser().parseFunDef(code)
      typed <- types.doValidation(typingScope)(tree)
    } yield typed

    result match
      case Right(TFunDef(id, ty, body, scope)) => BuiltinFunSpec(
        surfaceName = id.sourceName,
        stackType = ty,
        evaluationStrategy = de.cfaed.sigi.repl.eval(body),
        compilationStrategy = CompileFunctionDefinition(TFunDef(BuiltinFuncId(id.sourceName), ty, body, scope))
        )

      case value => throw new IllegalStateException("Compiling builtin failed " + value)
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
    val mlirFwdDeclaration = EmitMlirDefinition(name => s"func.func private $name(!sigi.stack) -> !sigi.stack attributes { sigi.builtinfunc }")

    val cond = // select one of two values
      stackFun("cond", StackType.generic1(ta => StackType(consumes = List(KBool, ta, ta), produces = List(ta))),
               compilationStrategy = FrontendIntrinsic) {
        case elseV :: thenV :: VPrimitive(KBool, condition) :: tl =>
          Right((if condition then thenV else elseV) :: tl)
      }

    val pass = stackFun("pass", StackType()) {
        case stack => Right(stack)
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
    // apply a function value
    val apply = stdLibFun()("let apply = -> \\f; f ;;")
    // compose the two functions at the top of the stack
    val compose = stdLibFun()("let compose = -> \\ab, \\bc; { ab bc } ;;")
    // swap top elements
    val swap = stdLibFun()("let swap = -> a, b; b a ;;")
    // quote top of the stack
    val quote = stdLibFun()("let quote = -> a; { a } ;;")
    // show an item and pop it. Implemented using pp.
    val show = stdLibFun(pp, pop)("let show = pp pop ;;")


    Set(
      // These operators need to be there because the parser refers to them.
      // Those bindings cannot be shadowed because they are not valid identifiers.
      binOp("+", _ + _),
      binOp("-", _ - _),
      binOp("*", _ * _),
      binOp("/", _ / _),
      binOp("%", _ % _),
      binOpStr("+", _ + _),
      cmpOp("<", _ < _),
      cmpOp(">", _ > _),
      cmpOp(">=", _ >= _),
      cmpOp("<=", _ <= _),
      genericCmpOp("=", false),
      genericCmpOp("<>", true),
      unaryOp("unary_-", a => -a),
      unaryOp("unary_+", a => a),
      unaryOp("unary_~", a => ~a),
      unaryOpStr("unary_+", a => a),

      boolOp("and", _ & _),
      boolOp("or", _ | _),
      boolOp("xor", _ ^ _),
      boolUnaryOp("not", !_),
      boolUnaryOp("unary_!", !_),

      // These are core function. Also see list of cat builtins: https://github.com/cdiggins/cat-language
      apply,
      compose,
      pop,
      dup,
      cond,
      pass,
      swap,
      quote,
      show,
      pp,
      )
  }
}
