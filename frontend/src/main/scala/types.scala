

package de.cfaed.kitten

package types {

  import ast.*
  import eval.Env
  import types.StackType.canonicalize

  import java.util.{Locale, Objects}
  import scala.annotation.tailrec
  import scala.collection.mutable
  import scala.util.Right
  import de.cfaed.kitten.{types, withFilter}

  import java.util


  /** Typed tree. */
  sealed trait TypedTree
  sealed trait TypedStmt extends TypedTree
  case class TFunDef(name: String, ty: StackType, body: TypedExpr) extends TypedStmt
  case class TExprStmt(e: TypedExpr) extends TypedStmt
  case class TBlock(stmts: List[TypedStmt]) extends TypedStmt

  sealed trait TypedExpr extends TypedTree {
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
      case TPushPrim(KString, value) => s"\"$value\""
      case TPushPrim(_, value) => value.toString
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

  /** Supertrait for KDataType and KRow[I]Var. */
  sealed trait KStackTypeItem {

    def isRowVarLike: Boolean = this match
      case _: KRowVar | _: KRowIVar => true
      case _ => false

    override def toString: String = this match
      case KInt => "int"
      case KString => "str"
      case KBool => "bool"
      case KFun(stackType) => "(" + stackType.toString + ")"
      case KList(item) => s"List[$item]"
      case tv: KTypeVar => tv.name
      case rv: KRowVar => rv.name
      case riv: KRowIVar => riv.origin.name + riv.id
      case iv: KInferenceVar => iv.origin.name + iv.id

    def exists(f: KStackTypeItem => Boolean): Boolean = {
      this match
        case a if f(a) => true
        case KFun(st) => st.consumes.exists(_.exists(f)) || st.produces.exists(_.exists(f))
        case KList(item) => item.exists(f)
        case _ => false
    }

    def contains(ivar: KInferenceVar): Boolean = exists(it => it == ivar)
  }

  /** Types of stack values. */
  sealed trait KDataType extends KStackTypeItem {

  }

  // both use identity semantics for Object::equals
  class KTypeVar(val name: String) extends KDataType
  object KTypeVar {
    private[types] def varNameGenerator(): () => String = {
      val names = "abcdefghijklmnopqrstuv"
      var i = 0
      () => {
        val k = i
        i += 1
        "'" + names(k % names.length)
      }
    }
    def typeVarGenerator(): () => KTypeVar = {
      // for some reason I can't write this with andThen
      val nameGen = varNameGenerator()
      () => KTypeVar(nameGen())
    }
  }

  class KInferenceVar(val origin: KTypeVar) extends KDataType {
    var instantiation: KDataType = _
    private[types] val id = {
      val id = KInferenceVar.seqNum
      KInferenceVar.seqNum += 1
      id
    }
  }
  object KInferenceVar {
    private var seqNum = 0
  }

  class KRowVar(val name: String) extends KStackTypeItem
  object KRowVar {
    def rowVarGenerator(): () => KRowVar = {
      // for some reason I can't write this with andThen
      val nameGen = KTypeVar.varNameGenerator()
      () => KRowVar(nameGen().toUpperCase(Locale.ROOT))
    }
  }
  class KRowIVar(val origin: KRowVar) extends KStackTypeItem {
    var instantiation: List[KStackTypeItem] = _
    val aliases = mutable.Set[KRowIVar]()
    private[types] val id = {
      val id = KRowIVar.seqNum
      KRowIVar.seqNum += 1
      id
    }
  }
  object KRowIVar {
    private var seqNum = 0
  }


  sealed trait KPrimitive[T] extends KDataType
  case object KInt extends KPrimitive[Int]
  case object KBool extends KPrimitive[Boolean]
  case object KString extends KPrimitive[String]

  /** An unapplied function type. */
  case class KFun(stack: StackType) extends KDataType
  case class KList(item: KDataType) extends KDataType


  /** Types of a term, a stack function. */
  case class StackType(consumes: List[KStackTypeItem] = Nil,
                       produces: List[KStackTypeItem] = Nil) {

    override def toString: String = (consumes.mkString(", ") + " -> " + produces.mkString(", ")).trim

    def exists(f: KStackTypeItem => Boolean): Boolean =
      consumes.exists(_.exists(f)) || produces.exists(_.exists(f))

    /** Introduce new trivial row variables if needed.
      * Turns `a -> b` into `'A, a -> 'A, b` if A and B
      * do not contain row variables.
      */
    def canonicalize: StackType =
      if this.consumes.exists(_.isRowVarLike) || this.produces.exists(_.isRowVarLike)
      then this
      else
        val rv = KRowVar("'S")
        StackType(
          consumes = rv :: this.consumes,
          produces = rv :: this.produces
        )

    /** Makes trivial row variables disappear. */
    def simplify: StackType = this match
      case StackType((t: KRowVar) :: ctl, (s: KRowVar) :: ptl)
        // the only occurrence of this var is t and s
        if t == s && !StackType(ctl, ptl).exists(_ == t) =>
        StackType(ctl, ptl)
      case other => other

    def map(f: KStackTypeItem => KStackTypeItem): StackType = StackType(
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
    def generic2[T](f: (KTypeVar, KTypeVar) => T): T = generic(newTypeVar => f(newTypeVar(), newTypeVar()))

    // Relabel distinct tvars from the start of the alphabet.
    def canonicalize(st: StackType): StackType =
      val tvars: mutable.Map[KTypeVar, KTypeVar] = mutable.Map()
      val tvarMaker = KTypeVar.typeVarGenerator()
      TySubst {
        case tv: KTypeVar => tvars.getOrElseUpdate(tv, tvarMaker())
        case _: KInferenceVar => throw IllegalStateException("ivars should not be present in grounded terms")
      }.substStackType(st)
  }


  /** Helper class to perform substitution on terms and types. */
  class TySubst(protected val f: (KTypeVar | KInferenceVar) => KDataType) {


    // generalize the parameter fun to apply to all types
    def substDataTy(t: KDataType): KDataType = t match
      case t: KTypeVar => f(t)
      case t: KInferenceVar => f(t)
      case KFun(st) => KFun(substStackType(st))
      case KList(item) => KList(substDataTy(item))
      case t => t

    def substStackType(stackType: StackType): StackType = stackType.map(applyEltWiseSubst)

    protected def applyEltWiseSubst(t: KStackTypeItem): KStackTypeItem = t match
      case a: KDataType => substDataTy(a)
      case a => a

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

    /** Replace type vars with fresh inference vars. */
    def mapToIvars(st: StackType): StackType = {
      val toIvars = mutable.Map[KTypeVar, KInferenceVar]()
      val toRivars = mutable.Map[KRowVar, KRowIVar]()
      new TySubst({
        case t: KTypeVar => toIvars.getOrElseUpdate(t, KInferenceVar(t))
        case t => t
      }) {
        override protected def applyEltWiseSubst(t: KStackTypeItem): KStackTypeItem = t match
          case t: KRowVar => toRivars.getOrElseUpdate(t, KRowIVar(t))
          case t => super.applyEltWiseSubst(t)
      }.substStackType(st)
    }


    private class Ground {
      private[this] val tvGen = KTypeVar.typeVarGenerator()
      private[this] val rvGen = KRowVar.rowVarGenerator()

      private val groundImpl: KDataType => KDataType = TySubst {
        case t: KInferenceVar =>
          if t.instantiation != null then
            t.instantiation = groundImpl(t.instantiation)
          else
            t.instantiation = tvGen()
          t.instantiation
        case t => t
      }.substDataTy _


      def groundTerm(groundDt: KDataType => KDataType): TySubst = new TySubst(groundDt) {
        def groundList(lst: List[KStackTypeItem]): List[KStackTypeItem] =
          lst.flatMap {
            case rivar: KRowIVar =>
              rivar.instantiation =
                if rivar.instantiation != null
                then groundList(rivar.instantiation)
                else
                  val aliasedInst = rivar.aliases.find(_.instantiation != null).map(_.instantiation)
                  aliasedInst.getOrElse(List(rvGen()))
              rivar.instantiation
            case t => List(applyEltWiseSubst(t))
          }
        override def substStackType(stackType: StackType): StackType =
          StackType(
            consumes = groundList(stackType.consumes),
            produces = groundList(stackType.produces)
          ).simplify // notice this simplify

      }

      def groundTerm(te: TypedExpr): TypedExpr = groundTerm(groundImpl).substTerm(te)
    }


    /** Replace inference variables with their instantiation.
      * Uninstantiated variables are replaced by fresh type vars.
      */
    def ground(te: TypedExpr): TypedExpr = new Ground().groundTerm(te)
    def groundRowVars: TySubst = new Ground().groundTerm(t => t)

    def unify(a: StackType, b: StackType): Option[KittenTypeError] =
      val ac = mapToIvars(a.canonicalize)
      val bc = mapToIvars(b.canonicalize)
      unify(ac.produces, bc.produces).orElse(unify(ac.consumes, bc.consumes))

    /**
      * Unify both lists of types. This adds constraints on the inference variables to make them
      * unifiable.
      */
    def unify(a: List[KStackTypeItem], b: List[KStackTypeItem], matchSuffix: Boolean = false): Option[KittenTypeError] = {
      // println(s"unify: $a =:= $b")

      def makeAlias(r: KRowIVar, s: KRowIVar, inst: List[KStackTypeItem]): Unit = {
        r.aliases += s
        s.aliases += r
        r.instantiation = inst
        s.instantiation = inst
      }

      def unifyRIvar(rivar: KRowIVar, lst: List[KStackTypeItem]): Option[KittenTypeError] =
        lst match
          case List(k: KRowIVar) =>
            if k == rivar then None
            else if (k.instantiation != null && rivar.instantiation != null)
              unify(rivar.instantiation, k.instantiation)
            else
              val inst = Option(k.instantiation).getOrElse(rivar.instantiation)
              makeAlias(rivar, k, inst)
              None
          case tyList =>
            if rivar.instantiation == null then
              // println(s"$rivar := $tyList")
              rivar.instantiation = tyList
              None
            else
            // the instantiation
              unify(rivar.instantiation, tyList, matchSuffix)


      def unifyImpl(aReversed: List[KStackTypeItem], bReversed: List[KStackTypeItem]): Option[KittenTypeError] = {
        (aReversed, bReversed) match
          case ((hd1: KDataType) :: tl1, (hd2: KDataType) :: tl2) => unify(hd1, hd2).orElse(unifyImpl(tl1, tl2))
          case (List(rv: KRowIVar), lst) => unifyRIvar(rv, lst.reverse)
          case (lst, List(rv: KRowIVar)) => unifyRIvar(rv, lst.reverse)
          case (Nil, Nil) => None
          case _ => Some(KittenTypeError.cannotUnify(aReversed.reverse, bReversed.reverse))
      }

      // We want to match these term to term, starting from the top of the stack (hence the reverse call)
      unifyImpl(a.reverse, b.reverse)
    }


    def unify(a: KDataType, b: KDataType): Option[KittenTypeError] =
      // println(s"unify: $a =:= $b")
      if a == b then None
      else
        def unifyIvar(a: KInferenceVar, b: KDataType): Option[KittenTypeError] =
          if a.instantiation != null then
            return unify(a.instantiation, b)

          assert(!b.contains(a))
          // println(s"$a := $b")
          a.instantiation = b
          None

        (a, b) match
          case (x: KInferenceVar, y) => unifyIvar(x, y)
          case (y, x: KInferenceVar) => unifyIvar(x, y)
          case (KFun(st1), KFun(st2)) => unify(st1, st2)
          case (KList(dt1), KList(dt2)) => unify(dt1, dt2)
          case _ => Some(KittenTypeError.mismatch(a, b))
  }

  private[types] class TypingScope(private val bindings: BindingTypes, private[types] val ctx: TypingCtx) {
    def addBindings(names: BindingTypes): TypingScope =
      new TypingScope(
        bindings = this.bindings ++ names,
        ctx = this.ctx
      )

    def typeOf(name: String): Either[KittenTypeError, KDataType] =
      bindings.get(name).toRight(KittenTypeError.undef(name))
  }


  private def inferListType(scope: TypingScope)(itemTrees: List[TypedExpr]): Either[KittenTypeError, KList] = itemTrees match
    case Nil => Right(StackType.generic1(KList.apply)) // -> List['a]
    case hd :: tl =>
      // now there may be type inference involved to unify types
      val itemType = tl.foldLeft[Either[KittenTypeError, StackType]](Right(scope.ctx.mapToIvars(hd.stackTy))) {
        (either, newT) =>
          either.flatMap(leftT => {
            val rightT = scope.ctx.mapToIvars(newT.stackTy)
            scope.ctx.unify(leftT, rightT).toLeft(leftT)
              .left.map { _ => KittenTypeError.mismatch(leftT, newT.stackTy) }
          })
      }

      itemType.flatMap({
        case StackType(Nil, List(single: KDataType)) => Right(KList(single))
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
      val withIvars = scope.ctx.mapToIvars(genericTy)
      val ivars = withIvars.consumes.asInstanceOf[List[KInferenceVar]]
      // make bindings whose types are the ivars
      val newEnv = scope.addBindings(names.zip(ivars).toMap)
      Right((TNameTopN(withIvars, names), newEnv))

    case FunApply(name) =>
      @tailrec
      def typeFunApply(funT: KDataType): TFunApply = funT match
        case KFun(stackTy) => TFunApply(stackTy, name)
        // if the function is itself in inference, we need to resolve it
        case ivar: KInferenceVar if ivar.instantiation != null => typeFunApply(ivar.instantiation)
        case t => TFunApply(StackType.pushOne(t), name)

      scope.typeOf(name).map(typeFunApply).map((_, scope))

    case e@Chain(left, right) =>
      val toIvars = scope.ctx.mapToIvars _
      for {
        (leftTree, leftScope) <- assignTypeRec(scope)(left)
        (rightTree, rightScope) <- assignTypeRec(leftScope)(right)
        newEnv <- {
          val (ta, tb) = (toIvars(leftTree.stackTy.canonicalize), toIvars(rightTree.stackTy.canonicalize))


          scope.ctx.unify(ta.produces, tb.consumes)
            .toLeft({
              val ground = scope.ctx.groundRowVars
              val ta2 = ground.substStackType(ta)
              val tb2 = ground.substStackType(tb)

              val commonLen = math.min(ta2.produces.length, tb2.consumes.length)
              val (prod, _) = ta2.produces.splitAtRight(commonLen)
              val (cons, _) = tb2.consumes.splitAtRight(commonLen)

              StackType(consumes = cons ++ ta2.consumes, produces = prod ++ tb2.produces)
            })
            .map(st => TChain(st, leftTree, rightTree))
            .map(term => (term, rightScope))
        }
      } yield newEnv


}
