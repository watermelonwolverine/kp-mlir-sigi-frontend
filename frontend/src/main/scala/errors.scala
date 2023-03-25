package de.cfaed.sigi

import ast.{KExpr, NameTopN}
import eval.{Env, KValue}
import types.*


trait SigiError
trait SigiCompilationError extends SigiError
object SigiCompilationError {
  export ListOfErrors.apply as allOf
}

case class ListOfErrors(lst: List[SigiCompilationError]) extends SigiCompilationError
case class SigiParseError(msg: String) extends SigiCompilationError

object SigiParseError {
  def namesShouldBeUnique(node: NameTopN) = SigiParseError(
    s"Names should be unique: ${node.names}"
  )
}
case class SigiLexerError(msg: String) extends SigiCompilationError


case class SigiTypeError(msg: String) extends SigiCompilationError {
  var location: KExpr = _
}

object SigiTypeError {
  def undef(name: String): SigiTypeError = SigiTypeError(s"Undefined name '$name'")
  def mismatch(t1: StackType, t2: StackType): SigiTypeError = SigiTypeError(
    s"Mismatched stack types: $t1 is not compatible with $t2"
  )
  def mismatch(a: KDataType, b: KDataType) = SigiTypeError(
    s"Mismatched types: $a is not compatible with $b"
  )
  
  def cannotUnify(a: List[KStackTypeItem], b: List[KStackTypeItem]) = SigiTypeError(
    s"Mismatched types: $a is not compatible with $b"
  )

  def cannotBeListItem(t1: StackType): SigiTypeError = SigiTypeError(
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


case class SigiEvalError(msg: String) extends SigiError
object SigiEvalError {
  def undef(name: String): SigiEvalError = SigiEvalError(s"Undefined name '$name'")
  def stackTypeError(t: StackType, env: Env): SigiEvalError =
    SigiEvalError(s"Cannot evaluate function of type ($t) with stack ${env.stackToString}")
}
