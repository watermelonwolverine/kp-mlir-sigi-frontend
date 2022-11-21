package de.cfaed.kitten

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
package datamodel {

  import types.{KDataType, KList, KTypeVar, StackType}

  import java.lang.invoke.TypeDescriptor

  sealed trait TypeDescriptor {
    def tparms: List[KTypeVar]
  }
  case class Aggregate(override val tparms: List[KTypeVar], alts: List[TypeAlt]) extends TypeDescriptor
  case class TypeAlt(name: String, fields: List[KDataType])

  case object Primitive extends TypeDescriptor {
    override def tparms: List[KTypeVar] = Nil
  }
  case class TypeParm(tv: KTypeVar) extends TypeDescriptor {
    override def tparms: List[KTypeVar] = Nil
  }


  object TypeDescriptor {
    val Predefined: Map[String, TypeDescriptor] = Map(
      "int" -> Primitive,
      "bool" -> Primitive,
      "str" -> Primitive, // todo make List[char]
      "List" -> StackType.generic1(tv =>
        Aggregate(List(tv), List(
          TypeAlt("cons", List(tv, KList(tv))),
          TypeAlt("nil", Nil)
        )))
    )
  }

}
