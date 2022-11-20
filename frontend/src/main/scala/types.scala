

package de.cfaed.kitten

package types {

  import ast.*
  import eval.Env

  import de.cfaed.kitten.types.StackType.canonicalize

  import java.util.Objects
  import scala.annotation.tailrec
  import scala.collection.mutable

  /** Types of stack values. */
  sealed trait KDataType {
    override def toString: String = this match
      case KInt => "int"
      case KString => "str"
      case KBool => "bool"
      case KFun(stackType) => "(" + stackType.toString + ")"
      case KList(item) => s"List[$item]"
      case tv: KTypeVar => tv.name
      case iv: KInferenceVar => iv.origin.name + System.identityHashCode(this)

  }

  // both use identity semantics for Object::equals
  class KTypeVar(val name: String) extends KDataType
  object KTypeVar {
    def typeVarGenerator(): () => KTypeVar = {
      val names = "abcdefghijklmnopqrstuv"
      var i = 0
      () => {
        val k = i
        i += 1
        KTypeVar("'" + names(k % names.length))
      }
    }
  }
  class KInferenceVar(val origin: KTypeVar) extends KDataType {
    var instantiation: KDataType = _
  }

  sealed trait KPrimitive[T] extends KDataType
  case object KInt extends KPrimitive[Int]
  case object KBool extends KPrimitive[Boolean]
  case object KString extends KPrimitive[String]

  /** An unapplied function type. */
  case class KFun(stack: StackType) extends KDataType
  case class KList(item: KDataType) extends KDataType

  /** Types of a term, a stack function. */
  case class StackType(consumes: List[KDataType] = Nil,
                       produces: List[KDataType] = Nil) {

    override def toString: String = (consumes.mkString(", ") + " -> " + produces.mkString(", ")).trim

    def map(f: KDataType => KDataType): StackType = StackType(
      produces = this.produces.map(f),
      consumes = this.consumes.map(f),
    )

  }

  object StackType {
    def pushOne(d: KDataType): StackType = StackType(produces = List(d))
    def symmetric(types: List[KDataType]): StackType = StackType(types, types)
    def symmetric1(t: KDataType): StackType = symmetric(List(t))

    def generic(f: (() => KTypeVar) => StackType): StackType = {
      f(KTypeVar.typeVarGenerator())
    }

    def generic1(f: KTypeVar => StackType): StackType = generic(newTypeVar => f(newTypeVar()))

    // Relabel distinct tvars
    def canonicalize(st: StackType): StackType = {
      val tvars: mutable.Map[KTypeVar, KTypeVar] = mutable.Map()
      val tvarMaker = KTypeVar.typeVarGenerator()

      def doCanonizeStackT: StackType => StackType = _.map(doCanonize)

      def doCanonize(t: KDataType): KDataType = t match
        case tv: KTypeVar =>
          tvars.getOrElseUpdate(tv, tvarMaker())
        case _: KInferenceVar => throw IllegalStateException("ivars should not be present in grounded terms")
        case KFun(st) => KFun(doCanonizeStackT(st))
        case t => t

      doCanonizeStackT(st)
    }
  }


  val BinOpType = StackType(consumes = List(KInt, KInt), produces = List(KInt))
  val UnaryOpType = StackType(consumes = List(KInt), produces = List(KInt))

  private[types] class TypingCtx {

    /** Replace type vars with fresh inference vars. */
    def prepareInference(map: mutable.Map[KTypeVar, KInferenceVar])(ty: StackType): StackType = {

      def instImpl(t: KDataType): KDataType =
        t match
          case t: KTypeVar => map.getOrElseUpdate(t, KInferenceVar(t))
          case KFun(st) => KFun(st.map(instImpl))
          case t => t

      ty.map(instImpl)
    }

    /** Replace inference variables with their instantiation.
      * Uninstantiated variables are replaced by fresh type vars.
      */
    def ground(st: StackType, env: TypingScope): (StackType, TypingScope) = {
      val instMap = mutable.Map[KInferenceVar, KDataType]()
      val tvGen = KTypeVar.typeVarGenerator()

      def groundImpl(t: KDataType): KDataType =
        t match
          case t: KInferenceVar =>
            instMap.get(t) match
              case Some(inst) => inst
              case None =>
                val inst = Option(t.instantiation).map(groundImpl).getOrElse(tvGen())
                instMap.put(t, inst)
                inst
          case KFun(st) => KFun(st.map(groundImpl))
          case t => t

      val groundSt: StackType => StackType = _.map(groundImpl)

      val newScope = TypingScope(
        bindings = env.bindings.view.mapValues(groundSt).toMap,
      )
      (groundSt(st), newScope)
    }

    def unify(a: StackType, b: StackType): Boolean =
      @tailrec
      def unifyList(as: List[KDataType], bs: List[KDataType]): Boolean =
        (as, bs) match
          case (Nil, Nil) => true
          case (hd1 :: tl1, hd2 :: tl2) => unify(hd1, hd2) && unifyList(tl1, tl2)
          case _ => false

      unifyList(a.produces, b.produces) && unifyList(a.consumes, b.consumes)

    def unify(a: KDataType, b: KDataType): Boolean =
      if a == b then true
      else
        def unifyIvar(a: KInferenceVar, b: KDataType): Boolean =
          if a.instantiation != null then
            return unify(a.instantiation, b)

          a.instantiation = b
          true

        (a, b) match
          case (x: KInferenceVar, y) => unifyIvar(x, y)
          case (y, x: KInferenceVar) => unifyIvar(x, y)
          case (KFun(st1), KFun(st2)) => unify(st1, st2)
          case _ => false
  }

  case class TypingScope(bindings: Map[String, StackType])

  def computeType(env: TypingScope)(node: KExpr): Either[KittenTypeError, (StackType, TypingScope)] = node match
    case PushPrim(ty, _) => Right((StackType.pushOne(ty), env))
    case PushList(items) =>
      val itemTypes: Either[KittenTypeError, List[StackType]] = items.map(computeType(env)).partition(_.isLeft) match
        case (Nil, types) => Right(for (Right((t, _)) <- types) yield t)
        case (hd :: _, _) => Left(hd.left.get)


      val res: Either[KittenTypeError, StackType] = itemTypes match
        case Right(Nil) => Right(StackType.generic1(tv => StackType.pushOne(KList(tv)))) // -> List['a]
        case Right(hd :: tl) =>
          // now there may be type inference involved to unify types
          val ctx = TypingCtx()
          val toIvars = ctx.prepareInference(mutable.Map())
          val itemType = tl.foldLeft[Either[KittenTypeError, StackType]](Right(hd)) {
            (either, newT) =>
              either.flatMap(leftT => {
                val rightT = toIvars(newT)
                if ctx.unify(leftT, rightT) then Right(ctx.ground(leftT, env)._1)
                else Left(KittenTypeError.mismatch(hd, newT))
              })
          }

          itemType.map(canonicalize).flatMap({
            case StackType(Nil, List(single)) => Right(StackType.pushOne(KList(single)))
            case st => Left(KittenTypeError.cannotBeListItem(st))
          })
        case Left(v) => Left(v)

      res.map((_, env))

    case Quote(term) => computeType(env)(term).map(t => (StackType.pushOne(KFun(t._1)), env))
    case node@NameTopN(names) =>
      val st = node.stackType
      Right((
        st,
        env.copy(bindings = names.zip(st.consumes.map(StackType.pushOne)).toMap ++ env.bindings)
      ))
    case FunApply(name) =>
      env.bindings.get(name).toRight(KittenTypeError.undef(name)).map(st => (st, env))


    case e@Chain(left, right) =>
      val ctx = TypingCtx()
      val toIvars = ctx.prepareInference(mutable.Map())
      for {
        left <- computeType(env)(left)
        right <- computeType(left._2.copy(left._2.bindings.view.mapValues(toIvars).toMap))(right)
        newEnv <- {

          val (ta, tb) = (toIvars(left._1), toIvars(right._1))
          // println(s"Chain ($ta) ($tb) in env ${right._2.bindings -- Env.default.toSymbolic.bindings.keys}")

          @tailrec
          def chainTypeCheck(ctx: TypingCtx)(produced: List[KDataType],
                                             consumed: List[KDataType]): Either[KittenTypeError, StackType] = {
            (produced, consumed) match
              case (a :: atl, b :: btl) =>
                if (ctx.unify(a, b)) chainTypeCheck(ctx)(atl, btl)
                else Left(KittenTypeError.cannotApply(e, ta, tb, a, b))
              case (Nil, Nil) => Right(StackType(consumes = ta.consumes, produces = tb.produces)) // fully saturated call
              case (Nil, notApplied) => Right(StackType(consumes = ta.consumes ::: notApplied, produces = tb.produces))
              case (notConsumed, Nil) =>
                Right(StackType(
                  consumes = ta.consumes,
                  produces = notConsumed ::: tb.produces,
                ))
          }

          chainTypeCheck(ctx)(ta.produces, tb.consumes)
            .map(st => ctx.ground(st, right._2))
        }
      } yield newEnv


}
