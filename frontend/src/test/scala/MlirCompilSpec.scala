package de.cfaed.sigi

import eval.Env
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
      val expected = classOf[MlirCompilSpec].getResourceAsStream(s"mlirTests/$fileName.text")

      assert(source != null)
      assert(expected != null)
      val out = ByteArrayOutputStream()

      dumpmlir.doDumpMlir(PrintStream(out))(io.Source.fromInputStream(source))

      assertResult(io.Source.fromInputStream(expected).mkString("\n"))(out.toString())
    }
  }

  doTest("simpleTest")

}
