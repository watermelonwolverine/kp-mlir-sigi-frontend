

package de.cfaed.sigi

package types {

  import ast.*
  import repl.Env
  import types.StackType.canonicalize

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
  case class TFunDef(id: FuncId, ty: StackType, body: TypedExpr) extends TypedStmt
  case class TExprStmt(e: TypedExpr) extends TypedStmt
  case class TBlock(stmts: List[TypedStmt]) extends TypedStmt
  case class TModule(
    functions: Map[FuncId, TFunDef],
    mainExpr: TypedExpr
  )

  sealed trait TypedExpr extends TypedTree {
    def stackTy: StackType

    def erase: KExpr =
      val res = this match
        case TChain(_, a, b) => Chain(a.erase, b.erase)
        case TPushList(_, items) => PushList(items.map(_.erase))
        case TFunApply(_, name) => FunApply(name.sourceName)
        case TPushQuote(term) => Quote(term.erase)
        case TNameTopN(_, names) => NameTopN(names.map(_.sourceName))
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

  case class TNameTopN(override val stackTy: StackType, names: List[StackValueId]) extends TypedExpr

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
    private var _instantiation: List[KStackTypeItem] = _
    val aliases: mutable.Set[KRowIVar] = mutable.Set()
    private[types] val id = {
      val id = KRowIVar.seqNum
      KRowIVar.seqNum += 1
      id
    }

    def instantiation: List[KStackTypeItem] = this._instantiation

    def instantiation_=(inst: List[KStackTypeItem]): Unit = {
      this._instantiation = inst
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

    private def symmetric(types: List[KDataType]): StackType = StackType(types, types)

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

  private[types] class TypingCtx {

    def toIvarSubst: TySubst = {
      val toIvars = mutable.Map[KTypeVar, KInferenceVar]()
      val toRivars = mutable.Map[KRowVar, KRowIVar]()
      makeTySubst(
      { case t: KTypeVar => toIvars.getOrElseUpdate(t, KInferenceVar(t)) },
      { case t: KRowVar => toRivars.getOrElseUpdate(t, KRowIVar(t)) }
                                                                          )
    }
    /** Replace type vars with fresh inference vars. */
    def mapToIvars(st: StackType): StackType = toIvarSubst.substStackType(st)

    /** A ground substitution substitutes inference variables with their instantiation. */

    def groundSubst(eliminateIvars: Boolean): TySubst =
      new TySubst {
        private[this] val tvGen = KTypeVar.typeVarGenerator()
        private[this] val rvGen = KRowVar.rowVarGenerator()

        override protected def substIVar(ivar: KInferenceVar): KDataType =
          if ivar.instantiation != null
          then
            ivar.instantiation = substDataTy(ivar.instantiation)
            ivar.instantiation
          else if eliminateIvars then
            ivar.instantiation = tvGen()
            ivar.instantiation
          else
            ivar

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


    /** Replace inference variables with their instantiation.
      * Uninstantiated variables are replaced by fresh type vars.
      */
    def ground(te: TypedExpr): TypedExpr = groundSubst(eliminateIvars = true).substTerm(te)

    //    def groundRowVars: TySubst = new Ground().groundSubst(t => t)

    // only replace vars that have an instantiation
    def partialGround: TySubst = groundSubst(eliminateIvars = false)


    def unifyWithoutIvarConversion(a: StackType, b: StackType): Option[SigiTypeError] =
      unify(a.produces, b.produces).orElse(unify(a.consumes, b.consumes))

    def unify(a: StackType, b: StackType): Option[SigiTypeError] =
      val ac = mapToIvars(a.canonicalize)
      val bc = mapToIvars(b.canonicalize)
      unifyWithoutIvarConversion(ac, bc)

    /**
      * Unify both lists of types. This adds constraints on the inference variables to make them
      * unifiable.
      */
    def unify(a: List[KStackTypeItem], b: List[KStackTypeItem], matchSuffix: Boolean = false): Option[SigiTypeError] = {
      // println(s"unify: $a =:= $b")

      def makeAlias(r: KRowIVar, s: KRowIVar, inst: List[KStackTypeItem]): Unit = {
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
              unify(rivar.instantiation, tyList, matchSuffix)


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
                            private val typesInScope: Map[String, datamodel.TypeDescriptor],
                            private[types] val ctx: TypingCtx) {


    def addBindings(newBindings: IterableOnce[VarBinding]): TypingScope =
      new TypingScope(
        bindings = this.bindings ++ newBindings.iterator.map(binding => binding.funcId.sourceName -> binding),
        typesInScope = this.typesInScope,
        ctx = this.ctx
        )

    def addTypeInScope(moreTypes: IterableOnce[(String, datamodel.TypeDescriptor)]): TypingScope =
      new TypingScope(
        bindings = this.bindings,
        typesInScope = this.typesInScope ++ moreTypes,
        ctx = this.ctx
      )

    def getBinding(termName: String): Either[SigiTypeError, VarBinding] =
      bindings.get(termName).toRight(SigiTypeError.undef(termName))

    def resolveType(typeName: String): Either[SigiTypeError, datamodel.TypeDescriptor] =
      typesInScope.get(typeName).toRight(SigiTypeError.undef(typeName))

  }
  object TypingScope {
    def apply(varNs: BindingTypes, typeNs: Map[String, datamodel.TypeDescriptor]): TypingScope =
      new TypingScope(varNs, typeNs, TypingCtx())
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
        val typedFuns: Map[FuncId, TFunDef] = funs.collect { case fun@TFunDef(id, _, _) => (id, fun) }.toMap
        val fileEnv = env.addBindings(typedFuns.map { case (id, fun) => VarBinding(id, KFun(fun.ty)) })
        doValidation(fileEnv)(KExprStatement(file.mainExpr))
          .flatMap { case TExprStmt(te) => checkMainExprType(te).toLeft(te) }
          .map(te => TModule(functions = typedFuns, mainExpr = te))
    }
  }

  def checkMainExprType(term: TypedExpr): Option[SigiTypeError] =
    if term.stackTy.simplify.consumes.nonEmpty
    then Some(SigiTypeError.mainExprConsumesElements(term.stackTy).setPos(term.pos))
    else None

  def doValidation(env: TypingScope)(ast: KStatement): Either[SigiTypeError, TypedStmt] = {
    ast match
      case KBlock(stmts) =>

        val fileEnv = stmts.collect {
          case KFunDef(id, ty, _) => resolveFunType(env)(ty).map(VarBinding(id, _))
        }.flattenList.map(env.addBindings)

        fileEnv.flatMap(env => stmts.map(doValidation(env)).flattenList.map(TBlock.apply).map(_.setPos(ast.pos)))

      case KExprStatement(e) => types.assignType(env)(e).map(TExprStmt.apply).map(_.setPos(ast.pos))
      case f@KFunDef(id, ty, body) =>
        for {
          funTy <- types.resolveFunType(env)(ty)
          typedBody <- types.assignType(env.addBindings(List(VarBinding(id, funTy))))(body)
          _ <- types.checkCompatible(typedBody.stackTy, funTy.stackTy)
            .map(SigiTypeError.bodyTypeIsNotCompatibleWithSignature(typedBody.stackTy, funTy.stackTy, id.sourceName).setPos(f.pos).addCause)
            .toLeft(())
        } yield TFunDef(id, funTy.stackTy, typedBody).setPos(ast.pos)
  }

  /** Check that the given type is compatible with the bound.
    * This means that the type is at least as general as the bound.
    * This represents a subtyping relation between function types.
    *
    * To check this, the bound is treated as a ground type (its
    * type variables are treated as types, not variables). A round
    * of type inference then checks that there exists an instantiation
    * of the first type that is compatible with the bound.
    */
  def checkCompatible(ty: StackType, bound: StackType): Option[SigiTypeError] =
    val ctx = new TypingCtx()
    val tyAsIvars = ctx.mapToIvars(ty.canonicalize).pp("to ivars")
    ctx.unifyWithoutIvarConversion(tyAsIvars, bound.canonicalize)

  def resolveType(env: TypingScope)(t: AstType): Either[SigiTypeError, KStackTypeItem] = t match
    case ATypeCtor(name, tyargs) => env.resolveType(name).flatMap {
      case datamodel.TypeParm(tv) =>
        if tyargs.isEmpty
        then Right(tv)
        else Left(SigiTypeError(s"Type parameter $name cannot have type arguments"))
      case typeDesc =>
        if typeDesc.tparms.lengthCompare(tyargs) != 0 then
          Left(SigiTypeError(s"Expected ${typeDesc.tparms.length} type arguments, got ${tyargs.length}"))
        else (name, tyargs) match
          case ("list", List(item)) => resolveType(env)(item).flatMap {
            case dty: KDataType => Right(dty)
            case t => Left(SigiTypeError.cannotBeListItem(t))
          }.map(KList.apply)
          case ("int", Nil) => Right(KInt)
          case ("str", Nil) => Right(KString)
          case ("bool", Nil) => Right(KBool)
          case _ => ??? // todo create user type
    }
    case tv: ATypeVar => Right(KTypeVar(tv.name)) // here we create a new type var always. We will coalesce type vars later.
    case tv: ARowVar => Right(KRowVar(tv.name)) // here we create a new type var always. We will coalesce type vars later.
    case ft: AFunType => resolveFunType(env)(ft)

  def resolveFunType(env: TypingScope)(t: AFunType): Either[SigiTypeError, KFun] = {
    val AFunType(consumes, produces) = t
    for {
      cs <- consumes.map(resolveType(env)).flattenList
      ps <- produces.map(resolveType(env)).flattenList
    } yield {
      val st = StackType(consumes = cs, produces = ps)
      val canon = mergeIdenticalTvars(st)
      KFun(canon)
    }
  }

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
    assignTypeRec(env)(node).map(_._1).map(env.ctx.ground)

  private def assignTypeRec(scope: TypingScope)(node: KExpr): Either[SigiTypeError, (TypedExpr, TypingScope)] = node match
    case PushPrim(ty, v) => Right((TPushPrim(ty, v).setPos(node.pos), scope))
    case PushList(items) =>
      val result = for {
        trees <- items.map(assignTypeRec(scope)).flattenList
        listTy <- inferListType(scope)(trees.map(_._1))
      } yield (TPushList(listTy, trees.map(_._1)).setPos(node.pos), scope)

      result.left.map(_.setPos(node.pos))

    case Quote(term) => assignTypeRec(scope)(term).map(t => (TPushQuote(t._1).setPos(node.pos), scope))
    case NameTopN(names) =>
      val typeVarGen = KTypeVar.typeVarGenerator()
      val newBindings = names.map { name =>
        val pos = FilePos(position = node.pos, fileName = "") // TODO filename
        VarBinding(new StackValueId(name, pos), KInferenceVar(typeVarGen()))
      }

      // this is the most generic type for this construct: every name has a different ivar
      val withIvars = StackType(consumes = newBindings.map(_.ty))
      // make bindings whose types are the ivars
      val newEnv = scope.addBindings(newBindings)
      Right((TNameTopN(withIvars, newBindings.map(_.funcId).asInstanceOf[List[StackValueId]]).setPos(node.pos), newEnv))

    case FunApply(name) =>
      @tailrec
      def typeFunApply(funT: VarBinding): TFunApply = funT.ty match
        case KFun(stackTy) => TFunApply(stackTy, funT.funcId)
        // if the function is itself in inference, we need to resolve it
        case ivar: KInferenceVar if ivar.instantiation != null => typeFunApply(VarBinding(funT.funcId, ivar.instantiation))
        case t => TFunApply(StackType.pushOne(t), funT.funcId)

      scope.getBinding(name) match
        case Left(err) => Left(err.setPos(node.pos))
        case Right(ty) => Right((typeFunApply(ty).setPos(node.pos), scope))

    case OpaqueExpr(te) => Right((TEvalBarrier(te).setPos(node.pos), scope))
    case Chain(left, right) =>
      for {
        (leftTree, leftScope) <- assignTypeRec(scope)(left)
        (rightTree, rightScope) <- assignTypeRec(leftScope)(right)
        newEnv <- {
          def toIvarSubst = scope.ctx.toIvarSubst

          val (leftI, rightI) = (toIvarSubst.substTerm(leftTree), toIvarSubst.substTerm(rightTree))
          val (ta, tb) = (toIvarSubst.substStackType(leftI.stackTy.canonicalize), toIvarSubst.substStackType(rightI.stackTy.canonicalize))

          scope.ctx.unify(ta.produces, tb.consumes).map(_.setPos(node.pos))
            .toLeft({
              val ground = scope.ctx.partialGround
              val ta2 = ground.substStackType(ta)
              val tb2 = ground.substStackType(tb)
              // println(s"$ta2 then $tb2")

              val commonLen = math.min(ta2.produces.length, tb2.consumes.length)
              val (prod, _) = ta2.produces.splitAtRight(commonLen)
              val (cons, _) = tb2.consumes.splitAtRight(commonLen)

              val stackTy = StackType(consumes = cons ++ ta2.consumes,
                                      produces = prod ++ tb2.produces)
              // println(s": $stackTy")
              TChain(stackTy, ground.substTerm(leftI), ground.substTerm(rightI)).setPos(node.pos)
            })
            .map(term => (term, rightScope))
        }
      } yield newEnv


}
