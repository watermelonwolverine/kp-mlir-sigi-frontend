package de.cfaed.sigi

import de.cfaed.sigi.debug.NoopLogger
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, PrintStream}
import scala.io.Source
import scala.util.parsing.input.NoPosition

/**
  * @author Clément Fournier &lt;clement.fournier@tu-dresden.de&gt;
  */
class CompilerErrorTest extends AnyFunSuite:
  case class MsgSpec(line: Int, column: Int, message: String, msgType: MsgType) {
    def doMatch(err: SigiError): Unit = {
      assertResult(message)(err.shortMessage)
      if (err.pos != NoPosition) {
        assertResult(line)(err.pos.line)
        assertResult(column)(err.pos.column)
      }
    }
  }

  inline def doTest(fileName: String)(processSource: Source => Option[SigiError]): Unit = {
    test(s"$fileName.sigi") {

      def getSource: Source = {
        val sourceStream = classOf[MlirCompilSpec].getResourceAsStream(s"errorTests/$fileName.sigi")
        io.Source.fromInputStream(sourceStream)
      }

      // comments that match this represent compiler messages
      val commentRegex = "((?:#|//)\\s*+)([\\^v]<?)\\s+(warn|error):(.*)$".r

      val specs = getSource.getLines().zipWithIndex.flatMap { (line, lineNo) =>
        commentRegex.findFirstMatchIn(line).map { rmatch =>
          val columnNo = rmatch.start + rmatch.group(1).length + 1 // cols are 1-based
          val (actualLine, actualCol) = rmatch.group(2) match
            case "^" => (lineNo, columnNo)
            case "v" => (lineNo + 2, columnNo)
            // These variants allow matching on the first column,
            // which is taken by the comment delimiter.
            case "^<" => (lineNo, 1)
            case "v<" => (lineNo + 2, 1)
          val message = rmatch.group(4).trim
          val kind = rmatch.group(3) match
            case "warn" => MsgType.Warning
            case "error" => MsgType.Error

          MsgSpec(actualLine, actualCol, message, kind)
        }
      }.toList


      (processSource(getSource), specs) match
        case (Some(error: SigiError), Nil) =>
          fail("Expected no errors, but got " + error)
        case (Some(error: SigiError), specs) =>
          val allErrors = error.allErrors
          allErrors.sortBy(_.pos).foreach { err =>
            specs.iterator.find(e => e.line == err.pos.line) match
              case Some(spec) => spec.doMatch(err)
              case None => fail(s"Unexpected error at line ${err.pos.line}: $err, expected " + specs)
          }
        case (None, Nil) => // everything's fine
        case (None, specs) =>
          fail("No error reported, expected " + specs)
    }
  }

  inline def doCompilationTest(fileName: String): Unit = {
    doTest(fileName) { source =>
      val os = new ByteArrayOutputStream()
      val ps = new PrintStream(os)
      emitmlir.parseSigiAndEmitMlir(ps)(source)(using NoopLogger)
    }
  }

  doCompilationTest("fwdRefs")
  doCompilationTest("inference")
  doCompilationTest("parseError0")


enum MsgType:
  case Error, Warning
