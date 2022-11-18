

package de.cfaed.kitten

package types {

  import de.cfaed.kitten.ast.*

  import scala.annotation.tailrec

  sealed trait KDataType

  case object KInt extends KDataType

  case object KString extends KDataType

  case class KFun(parms: List[KDataType], result: KDataType) extends KDataType


  case class StackType(consumes: List[KDataType],
                       produces: List[KDataType]) {

    override def toString: String = {
      consumes.mkString(", ") + " -> " + produces.mkString(", ")
    }


  }

  object StackType {
    def push(d: KDataType) = StackType(consumes = Nil, produces = List(d))
  }


  val BinOpType = StackType(consumes = List(KInt, KInt), produces = List(KInt))

  def dotype(node: KExpr): Either[KittenTypeError, StackType] = node match
    case Number(_) => Right(StackType.push(KInt))
    case Var(name) => {
      val BinOp = "[*+-/%]".r
      name match
        case BinOp() => Right(BinOpType)
        case _ => Right(StackType.push(KInt)) // TODO
    }
    case e@Chain(left, right) => {
      (dotype(left), dotype(right)) match
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
        case (a, b) => a.swap.orElse(b.swap).swap
    }

}
