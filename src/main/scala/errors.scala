package de.cfaed.sigi

import ast.{FuncId, KExpr, NameTopN, UserFuncId}
import repl.{Env, KValue}
import types.*

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.{NoPosition, Position, Positional}


sealed abstract class SigiError(private val msg: String) extends Positional {
  private val causes = ListBuffer[SigiError]()

  protected def errorType: String

  private def causesToString(sb: StringBuilder, indent: Int = 0): Unit = {
    causes.foreach { e =>
      sb.append(e.msg.indent(indent)).append("\n")
        .append(e.pos.longString.indent(indent)).append("\n")

      e.causesToString(sb, indent + 4)
    }
  }


  override def toString: String = {
    val sb = StringBuilder()
    sb.append(errorType).append(": ").append(msg).append("\n")
    if (pos != NoPosition)
      sb.append(pos.longString).append("\n")
    causesToString(sb)
    sb.toString()
  }

  def addCause(sigiError: SigiError): this.type = {
    causes += sigiError
    this
  }

  def addCauses(sigiError: IterableOnce[SigiError]): this.type = {
    causes ++= sigiError
    this
  }
}

class SigiCompilationError(msg: String) extends SigiError(msg) {
  override protected def errorType: String = "Compilation error"
}

object SigiCompilationError {
  export ListOfErrors.apply as allOf
}

case class ListOfErrors(lst: List[SigiCompilationError]) extends SigiCompilationError(lst.size + " errors occurred") {
  lst.foreach(this.addCause)
}

case class ListOfTypeErrors(lst: List[SigiTypeError]) extends SigiTypeError(lst.size + " errors occurred") {
  lst.foreach(this.addCause)
}

class SigiParseError(val msg: String) extends SigiCompilationError(msg) {
  override protected def errorType: String = "Parse error"
}

object SigiParseError {
  def apply(msg: String, loc: Position): SigiParseError = new SigiParseError(msg).setPos(loc)

  def namesShouldBeUnique(node: NameTopN): SigiParseError = SigiParseError(
    s"Names should be unique: ${node.names}",
    node.pos
    )
}


class SigiTypeError(msg: String) extends SigiCompilationError(msg) {

  override protected def errorType: String = "Type error"
}

given typeErrorMerger: ((SigiTypeError, List[SigiTypeError]) => SigiTypeError) = _.addCauses(_)

given compilErrorMerger: ((SigiCompilationError, List[SigiCompilationError]) => SigiCompilationError) = _.addCauses(_)


object SigiTypeError {

  def apply(msg: String) = new SigiTypeError(msg)

  def undef(name: String): SigiTypeError = SigiTypeError(s"Undefined name '$name'")

  def bodyTypeIsNotCompatibleWithSignature(bodyTy: StackType, sig: StackType, funcName: String): SigiTypeError = SigiTypeError(
    s"Type of the body of $funcName is not compatible with declared type: expected $sig, got $bodyTy"
    )

  def ivarsShouldBeGround(ivars: IterableOnce[KInferenceVar]): SigiTypeError = SigiTypeError(
    s"Could not ground the following ivars: $ivars"
    )

  def illegalFwdReferenceToFunWithInferredType(funcId: FuncId): SigiTypeError = SigiTypeError(
    s"Illegal forward reference to function ${funcId.sourceName} with inferred type"
    )

  def illegalRecursionWithInferredType(funcId: FuncId): SigiTypeError = SigiTypeError(
    s"Illegal recursive call to function ${funcId.sourceName} with inferred type"
    )

  def mainExprConsumesElements(stackType: StackType): SigiTypeError = SigiTypeError(
    s"Main expression should not consume elements from the stack, but its type is ($stackType)"
    )

  def mismatch(actual: StackType, expected: StackType): SigiTypeError = SigiTypeError(
    s"Mismatched stack types: $actual is not compatible with $expected"
    )

  def mismatch(a: KDataType, b: KDataType): SigiTypeError = SigiTypeError(
    s"Mismatched types: $a is not compatible with $b"
    )

  def cannotUnify(a: List[KStackTypeItem], b: List[KStackTypeItem]): SigiTypeError = SigiTypeError(
    s"Mismatched types: $a is not compatible with $b"
    )

  def cannotBeListItem(t1: StackType | KStackTypeItem): SigiTypeError = SigiTypeError(
    s"Type cannot be a list item: $t1 (quote expression to box it)"
    )

  def cannotApply(e: KExpr,
                  t1: StackType,
                  t2: StackType,
                  a: KDataType,
                  b: KDataType): SigiTypeError = SigiTypeError(
    s"Mismatched types: $a is not compatible with $b"
  )
}


case class SigiEvalError(msg: String) extends SigiError(msg) {
  override protected def errorType: String = "Evaluation error"
}
object SigiEvalError {
  def undef(name: String): SigiEvalError = SigiEvalError(s"Undefined name '$name'")
  def stackTypeError(t: StackType, env: Env): SigiEvalError =
    SigiEvalError(s"Cannot evaluate function of type ($t) with stack ${env.stackToString}")
}
