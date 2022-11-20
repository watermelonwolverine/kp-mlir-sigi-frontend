

package de.cfaed.kitten

package types {

  import ast.*
  import eval.Env
  import types.StackType.canonicalize

  import java.util.Objects
  import scala.annotation.tailrec
  import scala.collection.mutable
  import scala.util.Right
  import de.cfaed.kitten.withFilter


  /** Typed tree. */
  sealed trait TypedExpr {
    def stackTy: StackType

    def erase: KExpr = this match
      case TChain(_, a, b) => Chain(a.erase, b.erase)
      case TPushList(_, items) => PushList(items.map(_.erase))
      case TFunApply(_, name) => FunApply(name)
      case TPushQuote(term) => Quote(term.erase)
      case TNameTopN(_, names) => NameTopN(names)
      case TPushPrim(ty, value) => PushPrim(ty, value)

    override def toString: String = this match
      case TChain(stackTy, a, b) => s"(($a $b) : $stackTy)"
      case TPushList(ty, items) => items.mkString("[", ", ", s"] : $ty")
      case TPushPrim(ty, value) => value.toString
      case TFunApply(stackTy, name) => s"($name : $stackTy)"
      case t@TPushQuote(term) => s"({$term} : ${t.stackTy})"
      case TNameTopN(stackTy, names) => names.zip(stackTy.consumes).map((s, dt) => s"$s: $dt").mkString("-> ", ", ", ";")
  }
  case class TChain(override val stackTy: StackType, a: TypedExpr, b: TypedExpr) extends TypedExpr
  case class TPushList(ty: types.KList, items: List[TypedExpr]) extends TypedExpr {
    override def stackTy: StackType = StackType.pushOne(ty)
  }
  case class TPushPrim[T](ty: types.KPrimitive[T], value: T) extends TypedExpr {
    override def stackTy: StackType = StackType.pushOne(ty)
  }
  case class TFunApply(override val stackTy: StackType, name: String) extends TypedExpr
  case class TPushQuote(term: TypedExpr) extends TypedExpr {
    override def stackTy: StackType = StackType.pushOne(KFun(term.stackTy))
  }
  case class TNameTopN(override val stackTy: StackType, names: List[String]) extends TypedExpr


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
      consumes = this.consumes.map(f),
      produces = this.produces.map(f),
    )

  }

  object StackType {
    def pushOne(d: KDataType): StackType = StackType(produces = List(d))
    def symmetric(types: List[KDataType]): StackType = StackType(types, types)
    def symmetric1(t: KDataType): StackType = symmetric(List(t))

    def generic[T](f: (() => KTypeVar) => T): T = {
      f(KTypeVar.typeVarGenerator())
    }

    def generic1[T](f: KTypeVar => T): T = generic(newTypeVar => f(newTypeVar()))

    // Relabel distinct tvars
    def canonicalize(st: StackType): StackType =
      val tvars: mutable.Map[KTypeVar, KTypeVar] = mutable.Map()
      val tvarMaker = KTypeVar.typeVarGenerator()
      TySubst {
        case tv: KTypeVar => tvars.getOrElseUpdate(tv, tvarMaker())
        case _: KInferenceVar => throw IllegalStateException("ivars should not be present in grounded terms")
      }.substStackType(st)
  }

  class TySubst[A <: (KTypeVar | KInferenceVar) => KDataType](private val f: A) {

    // generalize the parameter fun to apply to all types
    def substDataTy(t: KDataType): KDataType = t match
      case t: KTypeVar => f(t)
      case t: KInferenceVar => f(t)
      case KFun(st) => KFun(st.map(substDataTy))
      case KList(item) => KList(substDataTy(item))
      case t: KPrimitive[_] => t

    def substStackType(stackType: StackType): StackType = stackType.map(substDataTy)

    def substTerm(te: TypedExpr): TypedExpr =
      te match
        case TChain(stackTy, a, b) =>
          val sta = substTerm(a)
          val stb = substTerm(b)
          TChain(substStackType(stackTy), sta, stb)
        case TPushList(KList(ty), items) => TPushList(KList(substDataTy(ty)), items.map(substTerm))
        case prim@TPushPrim(_, _) => prim
        case TFunApply(stackTy, name) => TFunApply(substStackType(stackTy), name)
        case TPushQuote(term) => TPushQuote(substTerm(term))
        case TNameTopN(stackTy, names) => TNameTopN(substStackType(stackTy), names)

  }

  val BinOpType = StackType(consumes = List(KInt, KInt), produces = List(KInt))
  val UnaryOpType = StackType(consumes = List(KInt), produces = List(KInt))

  private[types] class TypingCtx {
    private val toIvars = mutable.Map[KTypeVar, KInferenceVar]()

    /** Replace type vars with fresh inference vars. */
    val mapToIvars: StackType => StackType =
      TySubst {
        case t: KTypeVar => toIvars.getOrElseUpdate(t, KInferenceVar(t))
        case t => t
      }.substStackType _


    private class Ground {
      private[this] val tvGen = KTypeVar.typeVarGenerator()

      val groundImpl: KDataType => KDataType = TySubst {
        case t: KInferenceVar =>
          if t.instantiation != null then
            t.instantiation = groundImpl(t.instantiation)
          else
            t.instantiation = tvGen()
          t.instantiation
        case t => t
      }.substDataTy _

      val groundTerm: TypedExpr => TypedExpr = TySubst(groundImpl).substTerm _
    }

    /** Replace inference variables with their instantiation.
      * Uninstantiated variables are replaced by fresh type vars.
      */
    //    def ground(st: StackType, env: TypingScope): (StackType, TypingScope) = {
    //      val g = new Ground()
    //      val newScope = TypingScope(
    //        bindings = env.bindings.view.mapValues(g.groundSt).toMap,
    //      )
    //      (g.groundSt(st), newScope)
    //    }

    /** Replace inference variables with their instantiation.
      * Uninstantiated variables are replaced by fresh type vars.
      */
    def ground(te: TypedExpr): TypedExpr = new Ground().groundTerm(te)
    def earlyGround(ivar: KInferenceVar): Either[KittenTypeError, KDataType] = {
      ivar.instantiation match
        case null => Left(KittenTypeError(s"Not enough type information to infer type of ${ivar.origin}"))
        case e: KInferenceVar => earlyGround(e)
        case t => Right(t)
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
          case (KList(dt1), KList(dt2)) => unify(dt1, dt2)
          case _ => false
  }

  class TypingScope(private val bindings: BindingTypes, private[types] val ctx: TypingCtx) {
    def addBindings(names: BindingTypes): TypingScope =
      new TypingScope(
        bindings = this.bindings ++ names,
        ctx = this.ctx
      )

    def typeOf(name: String): Either[KittenTypeError, KDataType] =
      bindings.get(name).toRight(KittenTypeError.undef(name))
  }


  private def inferListType(scope: TypingScope)(itemTrees: List[TypedExpr]): Either[KittenTypeError, KList] = itemTrees match
    case Nil => Right(StackType.generic1(KList)) // -> List['a]
    case hd :: tl =>
      // now there may be type inference involved to unify types
      val itemType = tl.foldLeft[Either[KittenTypeError, StackType]](Right(scope.ctx.mapToIvars(hd.stackTy))) {
        (either, newT) =>
          either.flatMap(leftT => {
            val rightT = scope.ctx.mapToIvars(newT.stackTy)
            if scope.ctx.unify(leftT, rightT) then Right(leftT)
            else Left(KittenTypeError.mismatch(leftT, newT.stackTy))
          })
      }

      itemType.flatMap({
        case StackType(Nil, List(single)) => Right(KList(single))
        case st => Left(KittenTypeError.cannotBeListItem(st))
      })

  type BindingTypes = Map[String, KDataType]

  /** Turn a [[KExpr]] into a [[TypedExpr]] by performing type inference. */
  def assignType(bindings: BindingTypes)(node: KExpr): Either[KittenTypeError, TypedExpr] =
    val ctx = TypingCtx()
    val scope = TypingScope(bindings, ctx)
    assignTypeRec(scope)(node).map(_._1).map(ctx.ground)

  private def assignTypeRec(scope: TypingScope)(node: KExpr): Either[KittenTypeError, (TypedExpr, TypingScope)] = node match
    case PushPrim(ty, v) => Right((TPushPrim(ty, v), scope))
    case PushList(items) =>
      for {
        trees <- items.map(assignTypeRec(scope)).flattenList
        listTy <- inferListType(scope)(trees.map(_._1))
      } yield (TPushList(listTy, trees.map(_._1)), scope)

    case Quote(term) => assignTypeRec(scope)(term).map(t => (TPushQuote(t._1), scope))
    case NameTopN(names) =>
      // this is the most generic type for this construct: every name has a different tvar
      val genericTy = StackType.generic(newTVar => {
        StackType(consumes = names.map(_ => newTVar()))
      })
      // turn tvars into ivars
      val typed = TNameTopN(scope.ctx.mapToIvars(genericTy), names)
      // make bindings whose types are the ivars
      val newEnv = scope.addBindings(names.zip(typed.stackTy.consumes).toMap)
      Right((typed, newEnv))

    case FunApply(name) =>
      @tailrec
      def typeFunApply(funT: KDataType): TFunApply = funT match
        case KFun(stackTy) => TFunApply(stackTy, name)
        // if the function is itself in inference, we need to resolve it
        case ivar: KInferenceVar if ivar.instantiation != null => typeFunApply(ivar.instantiation)
        case t => TFunApply(StackType.pushOne(t), name)

      scope.typeOf(name).map(typeFunApply).map((_, scope))

    case e@Chain(left, right) =>
      val toIvars = scope.ctx.mapToIvars
      for {
        (leftTree, leftScope) <- assignTypeRec(scope)(left)
        (rightTree, rightScope) <- assignTypeRec(leftScope)(right)
        newEnv <- {
          val (ta, tb) = (toIvars(leftTree.stackTy), toIvars(rightTree.stackTy))
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

          chainTypeCheck(scope.ctx)(ta.produces, tb.consumes)
            .map(st => (TChain(st, leftTree, rightTree), rightScope))
        }
      } yield newEnv


}
