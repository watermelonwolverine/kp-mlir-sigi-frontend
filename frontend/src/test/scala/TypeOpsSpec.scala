package de.cfaed.sigi

import eval.Env
import types.*

import org.scalatest.funsuite.AnyFunSuite

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
class TypeOpsSpec extends AnyFunSuite {

  inline def checkCanonicalization(expected: String)(ty: => StackType): Unit = {
    test(s"$ty should canonicalize to $expected") {
      assertResult(expected)(StackType.canonicalize(ty).toString)
    }
  }


  checkCanonicalization("'a, 'b -> 'b") {
    StackType.generic(f => {
      val (a, b) = (f(), f())
      StackType(consumes = List(b, a), produces = List(a))
    })
  }


}
