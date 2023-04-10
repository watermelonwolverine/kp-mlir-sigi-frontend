package de.cfaed.sigi

import repl.Env
import types.*

import de.cfaed.sigi.ast.KFile
import de.cfaed.sigi.debug.NoopLogger
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, PrintStream}
import scala.io.Source

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
class MlirCompilSpec extends AnyFunSuite {

  inline def doTest(fileName: String): Unit = {
    test(s"$fileName.sigi") {

      def normText(s:String) = s.trim.linesIterator.filter(!_.isBlank).mkString("\n")

      val source = classOf[MlirCompilSpec].getResourceAsStream(s"mlirTests/$fileName.sigi")
      val expected = classOf[MlirCompilSpec].getResourceAsStream(s"mlirTests/$fileName.txt")
      val expectedStr = io.Source.fromInputStream(expected).mkString

      val out = ByteArrayOutputStream()

      val error = emitmlir.parseSigiAndEmitMlir(PrintStream(out))(io.Source.fromInputStream(source))(using NoopLogger)
      assertResult(None)(error)

      assertResult(normText(expectedStr))(normText(out.toString))
    }
  }

  doTest("simpleTest")
  doTest("boolCmp")
  doTest("intExpr")
  doTest("builtinOps")
  doTest("fibonacci")
  doTest("monophi")
  doTest("monophi2")
  doTest("generic1")
  doTest("loopWithPass")

}
