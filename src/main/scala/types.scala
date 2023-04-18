

package de.cfaed.sigi

package types {

  import ast.{KFunDef, *}
  import repl.{Env, VFun}
  import types.StackType.canonicalize

  import com.sun.tools.classfile.TypeAnnotation.TargetType

  import java.util.{Locale, Objects}
  import scala.annotation.tailrec
  import scala.collection.{TraversableOnce, mutable}
  import scala.util.Right
  import de.cfaed.sigi.{types, withFilter}

  import java.util
  import scala.collection.mutable.ListBuffer
  import scala.util.parsing.input.Positional


  /** Typed tree. */
  sealed trait TypedTree extends Positional

  sealed trait TypedStmt extends TypedTree

  case class TFunDef(id: EmittableFuncId, ty: StackType, body: TypedExpr, scope: TypingCtx => TypingScope) extends TypedStmt {
    def toVarBinding: VarBinding = VarBinding(id, KFun(ty))

    def toEvaluatable: VFun = VFun(Some(id.sourceName), ty, repl.eval(body))
  }

  case class TExprStmt(e: TypedExpr) extends TypedStmt

  case class TBlock(stmts: List[TypedStmt]) extends TypedStmt

  case class TModule(
    sourceFileName: String,
    functions: Map[FuncId, TFunDef],
    mainExpr: TypedExpr
  ) {
    def getFunction(id: FuncId): Option[TFunDef] = functions.get(id)
  }

  sealed trait TypedExpr extends TypedTree {
    def stackTy: StackType

    def erase: KExpr =
      val res = this match
        case TChain(_, a, b) => Chain(a.erase, b.erase)
        case TPushList(_, items) => PushList(items.map(_.erase))
        case TFunApply(_, name) => FunApply(name.sourceName)
        case TPushQuote(term) => Quote(term.erase)
        case TNameTopN(_, ids) => NameTopN(ids)
        case TPushPrim(ty, value) => PushPrim(ty, value)
        case TEvalBarrier(te) => OpaqueExpr(te)
      res.setPos(this.pos)

    def reduce[A](a: A)(f: PartialFunction[(TypedExpr, A), A]): A =
      val acc = f.applyOrElse((this, a), _._2)
      this match
        case TChain(_, a, b) =>
          val acc1 = a.reduce(acc)(f)
          b.reduce(acc1)(f)
        case TPushList(_, items) =>
          items.foldLeft(acc) { (acc, item) =>
            item.reduce(acc)(f)
          }
        case TPushQuote(term) => term.reduce(acc)(f)
        case _ => acc


    override def toString: String = this match
      case TChain(stackTy, a, b) => s"(($a $b) : $stackTy)"
      case TPushList(ty, items) => items.mkString("[", ", ", s"] : $ty")
      case TPushPrim(KString, value) => s"\"$value\""
      case TPushPrim(_, value) => value.toString
      case TFunApply(stackTy, id) => s"(${id.sourceName} : $stackTy)"
      case t@TPushQuote(term) => s"({$term} : ${t.stackTy})"
      case TNameTopN(stackTy, ids) => ids.map(_.sourceName).zip(stackTy.consumes).map((s, dt) => s"$s: $dt").mkString("-> ", ", ", ";")
      case TEvalBarrier(te) => te.toString
  }

  case class TChain(override val stackTy: StackType, a: TypedExpr, b: TypedExpr) extends TypedExpr

  case class TEvalBarrier(e: TypedExpr) extends TypedExpr {
    override def stackTy: StackType = e.stackTy
  }

  case class TPushList(ty: types.KList, items: List[TypedExpr]) extends TypedExpr {
    override def stackTy: StackType = StackType.pushOne(ty)
  }

  case class TPushPrim[T](ty: types.KPrimitive[T], value: T) extends TypedExpr {
    override def stackTy: StackType = StackType.pushOne(ty)
  }

  case class TFunApply(override val stackTy: StackType, binding: FuncId) extends TypedExpr

  case class TPushQuote(term: TypedExpr) extends TypedExpr {
    override def stackTy: StackType = StackType.pushOne(KFun(term.stackTy))
  }

  case class TNameTopN(override val stackTy: StackType, ids: List[StackValueId]) extends TypedExpr

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
      case KList(item) => s"$item list"
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

  enum BoundKind(val symbol: String) {
    case Lower extends BoundKind(">:")
    case Eq extends BoundKind("=")
    case Upper extends BoundKind("<:")

    def complement: BoundKind = this match
      case Lower => Upper
      case Eq => Eq
      case Upper => Lower
  }

  class KInferenceVar(val origin: KTypeVar, private val ctx: TypingCtx) extends KDataType {
    private val bounds: mutable.Map[BoundKind, mutable.Set[KStackTypeItem]] = mutable.Map()
    private var _instantiation: KDataType = _

    def instantiation: KDataType = _instantiation

    def addBound(kind: BoundKind, t: KStackTypeItem): Unit = {
      if bounds.getOrElseUpdate(kind, mutable.Set.empty).add(t) then
        ctx.onBoundAdded(this, kind, t)
    }

    def substInBounds(subst: TySubst): Unit = {
      bounds.mapValuesInPlace { (kind, boundSet) =>
        boundSet.map(subst.substTy)
      }
    }

    def instantiation_=(dt: KDataType): Unit = {
      this._instantiation = dt
      ctx.log.ivarInstantiated(this, dt)
    }

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

  class KRowIVar(val origin: KRowVar, val ctx: TypingCtx) extends KStackTypeItem {
    private var _instantiation: List[KStackTypeItem] = _
    val aliases: mutable.Set[KRowIVar] = mutable.Set()
    // sequential number
    private[sigi] val id = {
      val id = KRowIVar.seqNum
      KRowIVar.seqNum += 1
      id
    }

    def instantiation: List[KStackTypeItem] = this._instantiation

    def instantiation_=(inst: List[KStackTypeItem]): Unit = {
      if (inst != this.instantiation && inst != null) {
        this._instantiation = inst
        this.ctx.log.rivarInstantiated(this, inst)
      }
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
  case class KFun(stackTy: StackType) extends KDataType

  case class KList(item: KDataType) extends KDataType


  /** Types of a term, a stack function. */
  case class StackType(consumes: List[KStackTypeItem] = Nil,
                       produces: List[KStackTypeItem] = Nil) {

    override def toString: String = (consumes.mkString(", ") + " -> " + produces.mkString(", ")).trim

    private def exists(f: KStackTypeItem => Boolean): Boolean =
      consumes.exists(_.exists(f)) || produces.exists(_.exists(f))

    /** Whether this type declares some type variables. */
    // todo should row vars really be considered here?
    def isGeneric: Boolean = exists(t => t.isRowVarLike || t.isInstanceOf[KTypeVar])

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
          consumes = rv :: this.consumes.map(canonicalizeRec),
          produces = rv :: this.produces.map(canonicalizeRec)
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

    private def canonicalizeRec(t: KStackTypeItem) = t match
      case KFun(st) => KFun(st.canonicalize)
      case t => t
  }

  object StackType {
    def pushOne(d: KDataType): StackType = StackType(produces = List(d))

    private def symmetric(types: List[KDataType]): StackType = StackType(types, types)

    def symmetric1(t: KDataType): StackType = symmetric(List(t))

    def aToB(a: KStackTypeItem, b: KStackTypeItem): StackType = StackType(List(a), List(b))

    // Type ('A -> 'B)
    def anyFunction: StackType = {
      val rowVarGen = KRowVar.rowVarGenerator()
      StackType(List(rowVarGen()), List(rowVarGen()))
    }

    def generic[T](f: (() => KTypeVar) => T): T = {
      f(KTypeVar.typeVarGenerator())
    }

    def generic1[T](f: KTypeVar => T): T = generic(newTypeVar => f(newTypeVar()))

    def generic2[T](f: (KTypeVar, KTypeVar) => T): T = generic(newTypeVar => f(newTypeVar(), newTypeVar()))

    // Relabel distinct tvars from the start of the alphabet.
    def canonicalize(st: StackType): StackType =
      val tvars: mutable.Map[KTypeVar, KTypeVar] = mutable.Map()
      val tvarMaker = KTypeVar.typeVarGenerator()

      new TySubst() {
        override def substIVar(tvar: KInferenceVar): KDataType =
          throw IllegalStateException("ivars should not be present in grounded terms")

        override protected def substTVar(tvar: KTypeVar): KDataType =
          tvars.getOrElseUpdate(tvar, tvarMaker())
      }.substStackType(st)
  }

  def makeTySubst(
    tvarSubst: PartialFunction[KTypeVar | KInferenceVar, KDataType],
    rvarSubst: PartialFunction[KRowVar | KRowIVar, KStackTypeItem] = { t => t }
  ): TySubst =
    new TySubst() {
      override protected def substTVar(tvar: KTypeVar): KDataType = tvarSubst.applyOrElse(tvar, a => a)

      override protected def substIVar(tvar: KInferenceVar): KDataType = tvarSubst.applyOrElse(tvar, a => a)

      override protected def applyEltWiseSubst(t: KStackTypeItem): KStackTypeItem = t match
        case dty: KDataType => super.applyEltWiseSubst(dty)
        case rowVar: KRowVar => rvarSubst.applyOrElse(rowVar, a => a)
        case iVar: KRowIVar => rvarSubst.applyOrElse(iVar, a => a)
    }

  /** Helper class to perform substitution on terms and types. */
  class TySubst {


    // generalize the parameter fun to apply to all types
    def substDataTy(t: KDataType): KDataType = t match
      case t: KTypeVar => substTVar(t)
      case t: KInferenceVar => substIVar(t)
      case KFun(st) => KFun(substStackType(st))
      case KList(item) => KList(substDataTy(item))
      case t => t


    protected def substTVar(tvar: KTypeVar): KDataType = tvar

    protected def substIVar(tvar: KInferenceVar): KDataType = tvar

    def substStackType(stackType: StackType): StackType = stackType.map(applyEltWiseSubst)

    final def substTy(ty: KStackTypeItem): KStackTypeItem = applyEltWiseSubst(ty)

    protected def applyEltWiseSubst(t: KStackTypeItem): KStackTypeItem = t match
      case a: KDataType => substDataTy(a)
      case a => a

    def substTerm(te: TypedExpr): TypedExpr =
      val res = te match
        case TChain(stackTy, a, b) =>
          val sta = substTerm(a)
          val stb = substTerm(b)
          TChain(substStackType(stackTy), sta, stb)
        case TPushList(KList(ty), items) => TPushList(KList(substDataTy(ty)), items.map(substTerm))
        case prim@TPushPrim(_, _) => prim
        case TFunApply(stackTy, name) => TFunApply(substStackType(stackTy), name)
        case TPushQuote(term) => TPushQuote(substTerm(term))
        case TNameTopN(stackTy, names) => TNameTopN(substStackType(stackTy), names)
        case TEvalBarrier(term) => TEvalBarrier(substTerm(term))
      res.setPos(te.pos)
  }

  def binOpType(t: KDataType) = StackType(consumes = List(t, t), produces = List(t))

  def unaryOpType(t: KDataType) = StackType(consumes = List(t), produces = List(t))

  class TypingCtx(using debug.TypeInfLogger) {
    private[types] val log: debug.TypeInfLogger = implicitly

    private val myIvars = mutable.Set[KInferenceVar]()
    private val myRIvars = mutable.Set[KRowIVar]()

    private[types] def newIvar(typeVar: KTypeVar): KInferenceVar =
      val ivar = KInferenceVar(typeVar, this)
      myIvars += ivar
      ivar

    private[types] def newRIvar(rvar: KRowVar): KRowIVar =
      val rivar = KRowIVar(rvar, this)
      myRIvars += rivar
      rivar

    private[types] def onBoundAdded(ivar: KInferenceVar, boundKind: BoundKind, bound: KStackTypeItem): Unit = {
      log.boundAdded(ivar, boundKind, bound)
      bound match
        case ivar: KInferenceVar => ivar.addBound(boundKind.complement, ivar)
        case _ =>
    }

    /** Check that the given type is compatible with the bound.
      * This means that the type is at least as general as the bound.
      *
      * To check this, the bound is treated as a ground type (its
      * type variables are treated as types, not variables). A round
      * of type inference then checks that there exists an instantiation
      * of the first type that is compatible with the bound.
      */
    def checkCompatible(ty: StackType, bound: StackType): Option[SigiTypeError] =
      val tyAsIvars = mapToIvars(ty.canonicalize)
      unifyWithoutIvarConversion(tyAsIvars)(bound.canonicalize)


    /**
      * A substitution that maps all unknown type/row variables
      * to new ivars, rivars. The returned object has state:
      * reusing the same subst will reuse already known ivars.
      * This is sometimes wanted and sometimes to be avoided.
      */
    def toIvarSubst: TySubst = {
      val toIvars = mutable.Map[KTypeVar, KInferenceVar]()
      val toRivars = mutable.Map[KRowVar, KRowIVar]()
      makeTySubst(
      { case t: KTypeVar => toIvars.getOrElseUpdate(t, newIvar(t)) },
      { case t: KRowVar => toRivars.getOrElseUpdate(t, newRIvar(t)) })
    }

    /** Replace type vars with fresh inference vars. */
    def mapToIvars(st: StackType): StackType = toIvarSubst.substStackType(st)

    /** Returns an error if the ivars registered on this context are not all instantiatable. */
    private def checkAllIvarsGround(): Option[SigiTypeError] = {
      val freeIvars = myIvars.filter(_.instantiation == null)
      if freeIvars.nonEmpty then
        Some(SigiTypeError.ivarsShouldBeGround(freeIvars))
      else
        None
    }


    /** Given a ground callee type, and a generic callee type, infer
      * a substitution that maps the type variables within the generic type
      * to concrete data types found in the ground type.
      *
      * The ground callee type is known to be some instantiation of
      * the generic type because that's how it was derived during type inference.
      *
      * @param groundTy  The ground callee type
      * @param genericTy The generic callee type
      */
    def computeInstantiation(groundTy: StackType, genericTy: StackType): Either[SigiTypeError, TySubst] = {
      if !genericTy.isGeneric then return Right(makeTySubst({ t => t })) // empty substitution

      // otherwise infer the instantiations of each type var in the generic type
      val inferenceType = mapToIvars(genericTy)
      unifyWithoutIvarConversion(groundTy)(inferenceType)
        .toLeft({
          groundSubst(false)
        })
    }

    /** A ground substitution substitutes inference variables with their instantiation. */
    def groundSubst(eliminateIvars: Boolean): TySubst =
      new TySubst {
        private[this] val tvGen = KTypeVar.typeVarGenerator()
        private[this] val rvGen = KRowVar.rowVarGenerator()

        override protected def substIVar(ivar: KInferenceVar): KDataType =
          if ivar.instantiation != null
          then
            val groundInst = substDataTy(ivar.instantiation)
            if (groundInst != ivar.instantiation)
              ivar.instantiation = groundInst
            ivar.instantiation
          else if eliminateIvars then
            ivar.instantiation = tvGen()
            ivar.instantiation
          else
            ivar

        def groundList(lst: List[KStackTypeItem]): List[KStackTypeItem] =
          lst.flatMap {
            case rivar: KRowIVar =>
              if rivar.instantiation != null
              then
                if (rivar.instantiation.contains(rivar))
                  throw new IllegalStateException(s"$rivar instantiation contains itself: ${rivar.instantiation}")
                rivar.instantiation = groundList(rivar.instantiation)
                rivar.instantiation
              else if eliminateIvars then
                rivar.aliases.remove(rivar) // make sure we don't recurse infinitely
                val aliasedInst = rivar.aliases.find(_.instantiation != null).map(_.instantiation)
                val inst = aliasedInst.getOrElse(List(rvGen()))
                rivar.aliases.foreach(_.instantiation = inst)
                rivar.instantiation = inst
                inst
              else
                // only resolve aliases. Take the smallest one as the "canonical" one
                val smallestAlias = rivar.aliases.minByOption(_.id).filter(_.id < rivar.id).getOrElse(rivar)
                List(smallestAlias)

            case t => List(applyEltWiseSubst(t))
          }

        override def substStackType(stackType: StackType): StackType =
          StackType(
            consumes = groundList(stackType.consumes),
            produces = groundList(stackType.produces)
            ).simplify // notice this simplify

      }


    /** Replace inference variables with their instantiation.
      * Uninstantiated variables are replaced by fresh type vars.
      */
    def ground(te: TypedExpr): TypedExpr = groundSubst(eliminateIvars = true).substTerm(te)

    //    def groundRowVars: TySubst = new Ground().groundSubst(t => t)

    // only replace vars that have an instantiation
    def partialGround: TySubst = groundSubst(eliminateIvars = false)


    def unifyWithoutIvarConversion(a: StackType)(b: StackType): Option[SigiTypeError] =
      unify(a.produces, b.produces).orElse(unify(a.consumes, b.consumes))

    def unify(a: StackType, b: StackType): Option[SigiTypeError] =
      val ac = mapToIvars(a.canonicalize)
      val bc = mapToIvars(b.canonicalize)
      unifyWithoutIvarConversion(ac)(bc)

    /**
      * Unify both lists of types. This adds constraints on the inference variables to make them
      * unifiable.
      */
    def unify(a: List[KStackTypeItem], b: List[KStackTypeItem]): Option[SigiTypeError] = {
      log.subUnificationRequest(a, b)

      def makeAlias(r: KRowIVar, s: KRowIVar, inst: List[KStackTypeItem]): Unit = {
        log.recordAliasing(r, s, inst)
        r.aliases += s
        s.aliases += r
        r.instantiation = inst
        s.instantiation = inst
      }

      def unifyRIvar(rivar: KRowIVar, lst: List[KStackTypeItem]): Option[SigiTypeError] =
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
              unify(rivar.instantiation, tyList)


      def unifyImpl(aReversed: List[KStackTypeItem], bReversed: List[KStackTypeItem]): Option[SigiTypeError] = {
        (aReversed, bReversed) match
          case ((hd1: KDataType) :: tl1, (hd2: KDataType) :: tl2) => unify(hd1, hd2).orElse(unifyImpl(tl1, tl2))
          case (List(rv: KRowIVar), lst) => unifyRIvar(rv, lst.reverse)
          case (lst, List(rv: KRowIVar)) => unifyRIvar(rv, lst.reverse)
          case (Nil, Nil) => None
          case (hd1 :: tl1, hd2 :: tl2) if hd1 == hd2 => unifyImpl(tl1, tl2)
          case _ => Some(SigiTypeError.cannotUnify(aReversed.reverse, bReversed.reverse))
      }

      // We want to match these term to term, starting from the top of the stack (hence the reverse call)
      unifyImpl(a.reverse, b.reverse)
    }


    private def unify(a: KDataType, b: KDataType): Option[SigiTypeError] =
    // println(s"unify: $a =:= $b")
      if a == b then None
      else
        def unifyIvar(a: KInferenceVar, b: KDataType): Option[SigiTypeError] =
          if a.instantiation != null then
            return unify(a.instantiation, b)

          assert(!b.contains(a), s"$b contain $a")
          // println(s"$a := $b")
          a.instantiation = b
          None

        (a, b) match
          case (x: KInferenceVar, y) => unifyIvar(x, y)
          case (y, x: KInferenceVar) => unifyIvar(x, y)
          case (KFun(st1), KFun(st2)) => unify(st1, st2)
          case (KList(dt1), KList(dt2)) => unify(dt1, dt2)
          case _ => Some(SigiTypeError.mismatch(a, b))
  }

  case class VarBinding(funcId: FuncId, ty: KDataType)

  type BindingTypes = Map[String, VarBinding]

  class TypingScope private(private[types] val bindings: BindingTypes,

                            /** Map of incompletely typed IDs by name. Referencing them causes an error. */
                            private val incompletelyTypedThings: Map[String, (FuncId, FuncId => SigiTypeError)],
                            private val typesInScope: Map[String, datamodel.TypeDescriptor],
                            private[types] val ctx: TypingCtx) {


    def addIncompleteBinding(id: FuncId, errorMaker: FuncId => SigiTypeError): TypingScope =
      new TypingScope(
        bindings,
        incompletelyTypedThings + (id.sourceName -> (id, errorMaker)),
        typesInScope,
        ctx
        )

    def addIncompleteBindings(newItems: IterableOnce[(FuncId, FuncId => SigiTypeError)]): TypingScope =
      new TypingScope(
        bindings,
        incompletelyTypedThings ++ newItems.iterator.map(item => item._1.sourceName -> item),
        typesInScope,
        ctx
        )

    def promoteIncompleteBinding(binding: VarBinding): TypingScope =
      new TypingScope(
        bindings = this.bindings + (binding.funcId.sourceName -> binding),
        incompletelyTypedThings = this.incompletelyTypedThings - binding.funcId.sourceName,
        typesInScope = this.typesInScope,
        ctx = this.ctx
        )

    def withContext(typingCtx: TypingCtx): TypingScope = new TypingScope(
      bindings, incompletelyTypedThings, typesInScope, typingCtx
      )

    def addBindings(newBindings: IterableOnce[VarBinding]): TypingScope =
      new TypingScope(
        bindings = this.bindings ++ newBindings.iterator.map(binding => binding.funcId.sourceName -> binding),
        incompletelyTypedThings = this.incompletelyTypedThings,
        typesInScope = this.typesInScope,
        ctx = this.ctx
        )

    def addTypeInScope(moreTypes: IterableOnce[(String, datamodel.TypeDescriptor)]): TypingScope =
      new TypingScope(
        bindings = this.bindings,
        incompletelyTypedThings = this.incompletelyTypedThings,
        typesInScope = this.typesInScope ++ moreTypes,
        ctx = this.ctx
        )

    def getBinding(termName: String): Either[SigiTypeError, VarBinding] =
      bindings.get(termName).toRight(
        incompletelyTypedThings.get(termName).map(t => t._2(t._1))
          .getOrElse(SigiTypeError.undef(termName))
        )

    def resolveTypeName(typeName: String): Either[SigiTypeError, datamodel.TypeDescriptor] =
      typesInScope.get(typeName).toRight(SigiTypeError.undef(typeName))

  }
  object TypingScope {
    def apply(varNs: BindingTypes,
              typeNs: Map[String, datamodel.TypeDescriptor])
             (using debug.TypeInfLogger): TypingScope =
      new TypingScope(varNs, Map.empty, typeNs, TypingCtx())
  }


  private def inferListType(scope: TypingScope)(itemTrees: List[TypedExpr]): Either[SigiTypeError, KList] = itemTrees match
    case Nil => Right(StackType.generic1(KList.apply)) // -> List['a]
    case hd :: tl =>
      // now there may be type inference involved to unify types
      val itemType = tl.foldLeft[Either[SigiTypeError, StackType]](Right(scope.ctx.mapToIvars(hd.stackTy))) {
        (either, newT) =>
          either.flatMap(leftT => {
            val rightT = scope.ctx.mapToIvars(newT.stackTy)
            scope.ctx.unify(leftT, rightT).toLeft(leftT)
              .left.map { _ => SigiTypeError.mismatch(leftT, newT.stackTy).setPos(newT.pos) }
          })
      }

      itemType.flatMap({
        case StackType(Nil, List(single: KDataType)) => Right(KList(single))
        case st => Left(SigiTypeError.cannotBeListItem(st))
      })

  def typeFile(env: TypingScope)(file: KFile): Either[SigiTypeError, TModule] = {
    doValidation(env)(KBlock(file.funs)).flatMap {
      case TBlock(funs) =>
        val typedFuns: Map[FuncId, TFunDef] = funs.collect { case fun: TFunDef => fun.id -> fun }.toMap
        val fileEnv = env.addBindings(typedFuns.map { case (id, fun) => VarBinding(id, KFun(fun.ty)) })
        doValidation(fileEnv)(KExprStatement(file.mainExpr))
          .flatMap { case TExprStmt(te) => checkMainExprType(te).toLeft(te) }
          .map(te => TModule(sourceFileName = file.sourceFileName, functions = typedFuns, mainExpr = te))
    }
  }

  def checkCompatible(ty: StackType, bound: StackType)(using debug.TypeInfLogger): Option[SigiTypeError] =
    new TypingCtx().checkCompatible(ty, bound)

  def checkMainExprType(term: TypedExpr): Option[SigiTypeError] =
    if term.stackTy.simplify.consumes.nonEmpty
    then Some(SigiTypeError.mainExprConsumesElements(term.stackTy).setPos(term.pos))
    else None

  def doValidation(env: TypingScope)(ast: KStatement): Either[SigiTypeError, TypedStmt] =
    typeStatement(env)(ast).map(_._2)

  private def typeStatement(env: TypingScope)(ast: KStatement): Either[SigiTypeError, (TypingScope, TypedStmt)] = {
    ast match
      case KBlock(stmts) =>

        // first add all explicitly typed funs to the environment
        val explicitlyTypedDefs = stmts.collect {
          case KFunDef(id, Some(astTy), _) => resolveFunType(env)(astTy).map(VarBinding(id, _))
        }.flattenList.map(env.addBindings)


        var runningEnv = explicitlyTypedDefs match
          case Left(err) => return Left(err)
          case Right(defs) => defs

        // add incomplete bindings for better error messages
        stmts.foreach {
          case KFunDef(id, None, _) =>
            runningEnv = runningEnv.addIncompleteBinding(id, SigiTypeError.illegalFwdReferenceToFunWithInferredType)
          case _ =>
        }

        val typedStmts = ListBuffer[TypedStmt]()
        val errors = ListBuffer[SigiTypeError]()
        // then for each statement, type it and add its binding
        // to the env for the next statements
        for (stmt <- stmts) {
          typeStatement(runningEnv)(stmt) match
            case Left(error) =>
              errors += error
            case Right((e, tstmt)) =>
              typedStmts += tstmt
              runningEnv = e
        }

        errors.toList match
          case Nil => Right((env, TBlock(typedStmts.toList).setPos(ast.pos)))
          case List(e) => Left(e)
          case moreErrors => Left(ListOfTypeErrors(moreErrors))

      case KExprStatement(e) => types.assignType(env)(e).map(TExprStmt.apply).map(tstmt => (env, tstmt.setPos(ast.pos)))
      case f@KFunDef(id, optAstTy, body) =>
        for {
          // Option[Either[A, B]] => Either[A, Option[B]]
          // this is the explicit type
          funTy: Option[StackType] <- optAstTy.map(types.resolveFunType(env)).map(_.map(kfun => Some(kfun.stackTy))).getOrElse(Right(None))
          // If the type is present, then we add a regular binding.
          bodyEnv = funTy.map(ty => env.addBindings(List(VarBinding(id, KFun(ty)))))
            // Otherwise, recursive calls are illegal
            .getOrElse(env.addIncompleteBinding(id, SigiTypeError.illegalRecursionWithInferredType))

          typedBody <- types.assignType(bodyEnv)(body)
          // check that the type of the body is compatible with the signature
          _ <- funTy.flatMap {
            ty =>
              env.ctx.checkCompatible(typedBody.stackTy, ty)
                .map(SigiTypeError.bodyTypeIsNotCompatibleWithSignature(typedBody.stackTy, ty, id.sourceName).setPos(f.pos).addCause)
          }.toLeft(())
        } yield {
          val actualFunTy = funTy.getOrElse(typedBody.stackTy.simplify)
          (
            // the new env is augmented with a binding for this fun
            env.addBindings(List(VarBinding(id, KFun(actualFunTy)))),
            TFunDef(id, actualFunTy, typedBody, env.withContext).setPos(ast.pos)
          )
        }
  }

  def resolveType(env: TypingScope)(t: AstType): Either[SigiTypeError, KStackTypeItem] = t match
    case ATypeCtor(name, tyargs) => env.resolveTypeName(name).flatMap {
      case datamodel.TypeParm(tv) =>
        if tyargs.isEmpty
        then Right(tv)
        else Left(SigiTypeError(s"Type parameter $name cannot have type arguments").setPos(t.pos))
      case typeDesc =>
        if typeDesc.tparms.lengthCompare(tyargs) != 0 then
          Left(SigiTypeError(s"Expected ${typeDesc.tparms.length} type arguments, got ${tyargs.mkString(" ")}").setPos(t.pos))
        else (name, tyargs) match
          case ("list", List(item)) => resolveType(env)(item).flatMap {
            case dty: KDataType => Right(dty)
            case t => Left(SigiTypeError.cannotBeListItem(t).setPos(item.pos))
          }.map(KList.apply)
          case ("int", Nil) => Right(KInt)
          case ("str", Nil) => Right(KString)
          case ("bool", Nil) => Right(KBool)
          case _ => ??? // todo create user type
    }
    case tv: ATypeVar => Right(KTypeVar(tv.name)) // here we create a new type var always. We will coalesce type vars later.
    case tv: ARowVar => Right(KRowVar(tv.name)) // here we create a new type var always. We will coalesce type vars later.
    case ft: AFunType => resolveFunTypeRec(env)(ft)


  // this one does not merge tvars yet
  private def resolveFunTypeRec(env: TypingScope)(t: AFunType): Either[SigiTypeError, KFun] =
    val AFunType(consumes, produces) = t
    for {
      cs <- consumes.map(resolveType(env)).flattenList
      ps <- produces.map(resolveType(env)).flattenList
    } yield {
      val st = StackType(consumes = cs, produces = ps)
      KFun(st)
    }

  def resolveFunType(env: TypingScope)(t: AFunType): Either[SigiTypeError, KFun] =
    resolveFunTypeRec(env)(t).map(fun => mergeIdenticalTvars(fun.stackTy)).map(KFun.apply)

  private def mergeIdenticalTvars(st: StackType): StackType = {
    val tvGen = KTypeVar.typeVarGenerator()
    val rvGen = KRowVar.rowVarGenerator()
    val identicalTVars = mutable.Map[String, KTypeVar]()
    val identicalRVars = mutable.Map[String, KRowVar]()
    makeTySubst(
    { case tvar: KTypeVar => identicalTVars.getOrElseUpdate(tvar.name, tvGen()) },
    { case rvar: KRowVar => identicalRVars.getOrElseUpdate(rvar.name, rvGen()) })
      .substStackType(st).simplify
  }


  /** Turn a [[KExpr]] into a [[TypedExpr]] by performing type inference. */
  def assignType(env: TypingScope)(node: KExpr): Either[SigiTypeError, TypedExpr] =
    assignType(env)(None)(node)

  def assignType(env: TypingScope)(targetType: Option[StackType])(node: KExpr): Either[SigiTypeError, TypedExpr] =
    assignTypeRec(env)(node).map(_._1)
      .map(t => {
        // if there is a target type, then add new constraints before grounding
        targetType.foreach(env.ctx.unifyWithoutIvarConversion(t.stackTy))
        env.ctx.ground(t)
      })


  private def assignTypeRec(scope: TypingScope)(node: KExpr): Either[SigiTypeError, (TypedExpr, TypingScope)] = node match
    case PushPrim(ty, v) => Right((TPushPrim(ty, v).setPos(node.pos), scope))
    case PushList(items) =>
      val result = for {
        trees <- items.map(assignTypeRec(scope)).flattenList
        listTy <- inferListType(scope)(trees.map(_._1))
      } yield (TPushList(listTy, trees.map(_._1)).setPos(node.pos), scope)

      result.left.map(_.setPos(node.pos))

    case Quote(term) => assignTypeRec(scope)(term).map(t => (TPushQuote(t._1).setPos(node.pos), scope))
    case NameTopN(ids) =>
      val typeVarGen = KTypeVar.typeVarGenerator()
      val newBindings = ids.map { id =>
        val ty =
          if id.isFunction
          then KFun(scope.ctx.mapToIvars(StackType.anyFunction))
          else scope.ctx.newIvar(typeVarGen())
        VarBinding(id, ty)
      }

      // this is the most generic type for this construct: every name has a different ivar
      val withIvars = StackType(consumes = newBindings.map(_.ty))
      // make bindings whose types are the ivars
      val newEnv = scope.addBindings(newBindings)
      Right((TNameTopN(withIvars, newBindings.map(_.funcId).asInstanceOf[List[StackValueId]]).setPos(node.pos), newEnv))

    case FunApply(name) =>
      def typeFunApply(funT: VarBinding): TFunApply = funT match
        case VarBinding(id, KFun(stackTy)) => TFunApply(stackTy, id)
        case VarBinding(id, ty) => TFunApply(StackType.pushOne(ty), id)

      scope.getBinding(name) match
        case Left(err) => Left(err.setPos(node.pos))
        case Right(ty) => Right((typeFunApply(ty).setPos(node.pos), scope))

    case OpaqueExpr(te) => Right((TEvalBarrier(te).setPos(node.pos), scope))
    case Chain(left, right) =>
      for {
        (leftTree, leftScope) <- assignTypeRec(scope)(left)
        (rightTree, rightScope) <- assignTypeRec(leftScope)(right)
        newEnv <- {
          // This is a def so that a new subst is created each time.
          // For instance in the term `dup dup`, each `dup` call should
          // get its own ivars, even though the tvars are the same.
          def toIvarSubst = scope.ctx.toIvarSubst

          def log = scope.ctx.log

          log.startFrame(node)
          val (leftI, rightI) = (toIvarSubst.substTerm(leftTree), toIvarSubst.substTerm(rightTree))
          val (ta, tb) = (toIvarSubst.substStackType(leftI.stackTy.canonicalize), toIvarSubst.substStackType(rightI.stackTy.canonicalize))


          log.unificationRequest(ta, tb)
          scope.ctx.unify(ta.produces, tb.consumes).map(_.setPos(node.pos))
            .toLeft({
              // this only replaces ivars that have an instantiation but may keep other ivars live.
              val ground = scope.ctx.partialGround
              val ta2 = ground.substStackType(ta)
              val tb2 = ground.substStackType(tb)

              val commonLen = math.min(ta2.produces.length, tb2.consumes.length)
              val (prod, _) = ta2.produces.splitAtRight(commonLen)
              val (cons, _) = tb2.consumes.splitAtRight(commonLen)

              val stackTy = StackType(consumes = cons ++ ta2.consumes,
                                      produces = prod ++ tb2.produces)

              log.endFrameWithFinalType(stackTy)

              TChain(stackTy, ground.substTerm(leftI), ground.substTerm(rightI)).setPos(node.pos)
            })
            .map(term => (term, rightScope))
        }
      } yield newEnv


}
