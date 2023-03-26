package de.cfaed.sigi

import repl.{Env, KValue, VList, VNum}
import types.*
import org.scalatest.funsuite.AnyFunSuite

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
class EvalSpec extends AnyFunSuite {

  inline def checkResult(term: String, stack: List[KValue]): Unit = {
    test(s"$term should result in stack: $stack") {

      val env = Env.Default
      val result: Either[SigiError, Env] = for {
        parsed <- ast.SigiParser.parseExpr(term)
        typed <- types.assignType(env.toTypingScope)(parsed)
        env <- repl.eval(typed)(env)
      } yield env

      assertResult(Right(stack))(result.right.map(_.stack))
    }
  }

  checkResult("1 2", List(VNum(2), VNum(1)))
  checkResult("{->a,b; b} -> snd; 1 2 snd", List(VNum(2)))
  checkResult("{->a,b; b} -> snd; 1 (2 snd)", List(VNum(2)))

  checkResult("\"a\" 2 {->a,b; b} -> snd; snd", List(VNum(2)))
  checkResult("2 pop", List())
  checkResult("2 dup pop", List(VNum(2)))


  checkResult("if (true) { 1 } else { 2 }", List(VNum(1)))
  checkResult("[1, 2]", List(VList(types.KList(types.KInt), List(VNum(1), VNum(2)))))
}
