package de.cfaed.kitten

import eval.{Env, KValue, VNum}
import types.{StackType, TypingScope}

import org.scalatest.funsuite.AnyFunSuite

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
class EvalSpec extends AnyFunSuite {

  inline def checkResult(term: String, stack: List[KValue]): Unit = {
    test(s"$term should result in stack: $stack") {

      val result: Either[KittenError, Env] = for {
        parsed <- ast.KittenParser.parseExpr(term)
        typed <- types.assignType(Env.Default.bindingTypes)(parsed)
        env <- eval.eval(typed)(Env.Default)
      } yield env

      assertResult(Right(stack))(result.right.map(_.stack))
    }
  }

  checkResult("1 2", List(VNum(2), VNum(1)))
  checkResult("{->a,b; b} -> snd; 1 2 snd", List(VNum(2)))
  checkResult("{->a,b; b} -> snd; 1 (2 snd)", List(VNum(2)))

  checkResult("\"a\" 2 {->a,b; b} -> snd; snd", List(VNum(2)))
  checkResult("2 show", List())
  checkResult("2 pp", List(VNum(2)))


  checkResult("if (true) { 1 } else { 2 }", List(VNum(1)))
}
