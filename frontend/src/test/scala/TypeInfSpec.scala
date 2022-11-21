package de.cfaed.kitten

import eval.Env
import types.*


import org.scalatest.funsuite.AnyFunSuite

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
class TypeInfSpec extends AnyFunSuite {

  inline def checkType(term: String, ty: String): Unit = {
    test(s"$term should have type $ty") {

      val result: Either[KittenError, StackType] = for {
        parsed <- ast.KittenParser.parseExpr(term)
        typed <- types.assignType(Env.Default.bindingTypes)(parsed)
      } yield typed.stackTy

      assertResult(Right(ty))(result.right.map(_.toString))
    }
  }


  checkType("1 2", "-> int, int")
  checkType("1 2 pop", "-> int")
  checkType("{ pop }", "-> ('a ->)")

  checkType("-> x, y; x y", "'a, 'b -> 'a, 'b")
  checkType("-> x;", "'a ->")
  checkType("-> x, y; y", "'a, 'b -> 'b")
  checkType("if (true) { 1 } else { 2 }", "-> int")
  checkType("if { 1 } else { 2 }", "bool -> int")
  checkType("true if { 1 } else { 2 }", "-> int")
  checkType("{->a,b;b} -> snd; 1 2 snd", "-> int")

  checkType("1 2 -> x, y; x y", "-> int, int")
  checkType("(1 2 -> x, y;) x y", "-> int, int")
  checkType("1 2 ((-> x, y; x) y)", "-> int, int")
  checkType("1 2 (-> x, y; (x y))", "-> int, int")

  checkType("[]", "-> List['a]")
  checkType("[2]", "-> List[int]")
  checkType("[{->x; x}]", "-> List[('a -> 'a)]")
  checkType("-> x, y; [x, y]", "'a, 'a -> List['a]")
  checkType("-> x, y; x * y", "int, int -> int")
  checkType("-> x, y; x x * y", "int, int -> int, int")

  // this used to execute correctly but type to
  //  -> int, ('a, 'b -> 'b), str
  checkType("\"a\" 2 {->a,b; b} -> snd; snd", "-> int")
  // Same as previous. Given application of terms with
  // types (-> 'a, 'b) ('c ->), we should infer that 'b = 'c.
  // This is because the left term will push and 'a, then a 'b,
  // while the right term will pop a 'c.
  // When the arities match, we have (-> 'a, 'b) ('c, 'd ->)
  // where 'a = 'c and 'b = 'd. That's because consumed arguments
  // are popped in reverse.
  checkType("(1 true) pop", "-> int")


}
