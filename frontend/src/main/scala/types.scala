

package de.cfaed.kitten

package types {

  import ast.*

  import de.cfaed.kitten.eval.Env

  import java.util.Objects
  import scala.annotation.tailrec
  import scala.collection.mutable

  /** Types of stack values. */
  sealed trait KDataType {
    override def toString: String = this match
      case KInt => "int"
      case KString => "str"
      case KFun(stackType) => stackType.toString
      case tv: KTypeVar => tv.name
      case iv: KInferenceVar => iv.origin.name + System.identityHashCode(this)

  }

  // both use identity semantics for Object::equals
  class KTypeVar(val name: String) extends KDataType
  class KInferenceVar(val origin: KTypeVar) extends KDataType {
    var instantiation: KDataType = _
  }

  case object KInt extends KDataType
  case object KString extends KDataType

  /** An unapplied function type. */
  case class KFun(stack: StackType) extends KDataType

  /** Types of a term, a stack function. */
  case class StackType(consumes: List[KDataType] = Nil,
                       produces: List[KDataType] = Nil) {

    override def toString: String = consumes.mkString(", ") + " -> " + produces.mkString(", ")
  }

  object StackType {
    def pushOne(d: KDataType): StackType = StackType(produces = List(d))
    val empty: StackType = StackType()
    def generic(f: (() => KTypeVar) => StackType): StackType = {
      val names = "abcdefghijklmnopqrstuv"
      var i = 0
      f(() => {
        val k = i
        i += 1
        KTypeVar("'" + names(k % names.length))
      })
    }

    def generic1(f: KTypeVar => StackType): StackType = generic(newTypeVar => f(newTypeVar()))
  }


  val BinOpType = StackType(consumes = List(KInt, KInt), produces = List(KInt))

  private[types] class TypingCtx {
    private[types] val identities = mutable.Set[(KInferenceVar, KDataType)]()


    def instantiate(st: StackType): StackType = {
      val map = mutable.Map[KTypeVar, KInferenceVar]()


      def instImpl(t: KDataType): KDataType =
        t match
          case t: KTypeVar => map.getOrElseUpdate(t, KInferenceVar(t))
          case KFun(StackType(consumes, produces)) => KFun(StackType(
            consumes.map(instImpl), produces.map(instImpl)
          ))
          case t => t


      StackType(
        st.consumes.map(instImpl),
        st.produces.map(instImpl)
      )
    }

    def ground(st: StackType): StackType = {
      StackType(st.consumes.map(ground), st.produces.map(ground))
    }

    def ground(t: KDataType): KDataType = {

      def groundImpl(t: KDataType): KDataType =
        t match
          case t: KInferenceVar =>
            Option(t.instantiation).map(groundImpl).getOrElse(t.origin)
          case KFun(StackType(consumes, produces)) =>
            KFun(StackType(
              consumes = consumes.map(groundImpl),
              produces = produces.map(groundImpl)
            ))
          case t => t

      groundImpl(t)
    }

    def unify(a: KDataType, b: KDataType): Boolean =
      if a == b then true
      else
        def doUnify(a: KInferenceVar, b: KDataType): Boolean =
          if a.instantiation != null then
            return unify(a.instantiation, b)

          this.identities.add((a, b))
          if b.isInstanceOf[KInferenceVar] then
            this.identities.add((b.asInstanceOf[KInferenceVar], a))
          else
            a.instantiation = b
          true

        (a, b) match
          case (x: KInferenceVar, y) => doUnify(x, y)
          case (y, x: KInferenceVar) => doUnify(x, y)
          case _ => false //todo structured types
  }

  case class SymbolicEnv(val stack: List[KDataType], val bindings: Map[String, StackType]) {

  }

  def computeType(env: SymbolicEnv)(node: KExpr): Either[KittenTypeError, (StackType, SymbolicEnv)] = node match
    case Number(_) => Right((StackType.pushOne(KInt), env))
    case NameTop(name) =>
      val newEnv = env.copy(bindings = env.bindings.+((name, hd)))
      env.stack match
        case hd :: _ => Right((StackType(List(hd), List(hd)), env.copy(bindings = env.bindings.+((name, StackType.pushOne(hd))))))
        case _ => Right((StackType.generic1(t => StackType(List(t), List(t))), env)) // todo env should use a binding for "any type" or so
    case FunApply(name) =>
      env.bindings.get(name).toRight(KittenTypeError.undef(name))


    case e@Chain(left, right) =>
      for {
        left <- computeType(env)(left)
        right <- computeType(left._2)(right)
      } yield {
        val ctx = TypingCtx()

        val ta = ctx.instantiate(left._1)
        val tb = ctx.instantiate(right._1)

        @tailrec
        def chainTypeCheck(ctx: TypingCtx)(produced: List[KDataType],
                                           consumed: List[KDataType]): Either[KittenTypeError, StackType] = {
          (produced, consumed) match
            case (a :: atl, b :: btl) =>
              if (ctx.unify(a, b)) chainTypeCheck(ctx)(atl, btl)
              else Left(KittenTypeError.cannotApply(e, ta, tb, a, b))
            case (Nil, Nil) => Right(StackType(consumes = Nil, produces = tb.produces)) // fully saturated call
            case (Nil, notApplied) => Right(StackType(consumes = notApplied, produces = tb.produces))
            case (notConsumed, Nil) =>
              Right(StackType(
                consumes = Nil,
                produces = notConsumed ::: tb.produces,
              ))
        }

        chainTypeCheck(ctx)(
          ta.produces,
          tb.consumes
        ).map(ctx.ground)
      }

}
