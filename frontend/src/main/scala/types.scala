

package de.cfaed.kitten

package types {

  import ast.*

  import scala.annotation.tailrec

  /** Types of stack values. */
  sealed trait KDataType {
    override def toString: String = this match
      case KInt => "int"
      case KString => "str"
      case KFun(stackType) => stackType.toString
  }

  case object KInt extends KDataType

  case object KString extends KDataType

  /** An unapplied function type. */
  case class KFun(stack: StackType) extends KDataType

  /** Types of a term, a stack function. */
  case class StackType(consumes: List[KDataType],
                       produces: List[KDataType]) {

    override def toString: String = {
      consumes.mkString(", ") + " -> " + produces.mkString(", ")
    }
  }

  object StackType {
    def pushOne(d: KDataType): StackType = StackType(consumes = Nil, produces = List(d))
  }


  val BinOpType = StackType(consumes = List(KInt, KInt), produces = List(KInt))

  def computeType(node: KExpr): Either[KittenTypeError, StackType] = node match
    case Number(_) => Right(StackType.pushOne(KInt))
    case FunApply(name) => {
      val BinOp = "[*+-/%]".r
      name match
        case BinOp() => Right(BinOpType)
        case _ => Right(StackType.pushOne(KInt)) // TODO
    }
    case e@Chain(left, right) => {
      (computeType(left), computeType(right)) match
        case (Right(ta), Right(tb)) => {


          @tailrec
          def chainTypeCheck(produced: List[KDataType], consumed: List[KDataType]): Either[KittenTypeError, StackType] = {
            (produced, consumed) match
              case (a :: atl, b :: btl) =>
                if (a == b) chainTypeCheck(atl, btl)
                else Left(KittenTypeError.cannotApply(e, ta, tb, a, b))
              case (Nil, Nil) => Right(StackType(consumes = Nil, produces = tb.produces)) // fully saturated call
              case (Nil, notApplied) => Right(StackType(consumes = notApplied, produces = tb.produces))
              case (notConsumed, Nil) =>
                Right(StackType(
                  consumes = Nil,
                  produces = notConsumed ::: tb.produces,
                ))
          }

          chainTypeCheck(ta.produces, tb.consumes)
        }
        case (a, b) => a.swap.orElse(b.swap).swap // a left
    }

}
