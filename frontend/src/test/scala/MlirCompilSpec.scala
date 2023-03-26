package de.cfaed.sigi

import repl.Env
import types.*

import de.cfaed.sigi.ast.KFile
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, PrintStream}
import scala.io.Source

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
class MlirCompilSpec extends AnyFunSuite {

  inline def doTest(fileName: String): Unit = {
    test(s"$fileName.sigi") {

      val source = classOf[MlirCompilSpec].getResourceAsStream(s"mlirTests/$fileName.sigi")
      val expected = classOf[MlirCompilSpec].getResourceAsStream(s"mlirTests/$fileName.txt")
      val expectedStr = io.Source.fromInputStream(expected).mkString.trim

      val out = ByteArrayOutputStream()

      emitmlir.parseSigiAndEmitMlir(PrintStream(out))(io.Source.fromInputStream(source))

      assertResult(expectedStr)(out.toString().trim)
    }
  }

//  doTest("simpleTest")
//  doTest("boolCmp")
  doTest("fibonacci")

}
