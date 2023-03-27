package de.cfaed.sigi

import ast.{KExpr, NameTopN}
import repl.{Env, KValue}
import types.*

import scala.util.parsing.input.{Position, Positional}


sealed class SigiError extends Positional
class SigiCompilationError extends SigiError

object SigiCompilationError {
  export ListOfErrors.apply as allOf
}

case class ListOfErrors(val lst: List[SigiCompilationError]) extends SigiCompilationError {

}
class SigiParseError(val msg: String) extends SigiCompilationError {
  override def toString: String = s"Parse error: $msg\n" + pos.longString
}

object SigiParseError {
  def apply(msg: String, loc: Position): SigiParseError = new SigiParseError(msg).setPos(loc)
  def namesShouldBeUnique(node: NameTopN): SigiParseError = SigiParseError(
    s"Names should be unique: ${node.names}",
    node.pos
  )
}
case class SigiLexerError(msg: String) extends SigiCompilationError


class SigiTypeError(msg: String) extends SigiCompilationError {

  // val stackTrace = new Throwable(msg).printStackTrace()

  override def toString: String = s"Type error: $msg\n" + pos.longString
}

object SigiTypeError {

  def apply(msg: String) = new SigiTypeError(msg)

  def undef(name: String): SigiTypeError = SigiTypeError(s"Undefined name '$name'")
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


case class SigiEvalError(msg: String) extends SigiError
object SigiEvalError {
  def undef(name: String): SigiEvalError = SigiEvalError(s"Undefined name '$name'")
  def stackTypeError(t: StackType, env: Env): SigiEvalError =
    SigiEvalError(s"Cannot evaluate function of type ($t) with stack ${env.stackToString}")
}
