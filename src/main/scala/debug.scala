package de.cfaed.sigi

package debug {

  import java.io.PrintStream
  import types.*

  import de.cfaed.sigi.ast.KExpr
  import de.cfaed.sigi.debug.Coloring.ColorArray

  import scala.io.AnsiColor

  val debugTypeInfLogger = NoopLogger
  // val debugTypeInfLogger = DebugTypeInfLogger()

  class Coloring private(val useColors: Boolean) extends scala.io.AnsiColor {

    private def paint(color: String, string: String) =
      if useColors then s"$color$string$RESET" else string

    def paintWhite(string: Any): String = paint(WHITE, string.toString)

    def paintRed(string: Any): String = paint(RED, string.toString)

    def colorNameOfVar(rivar: KRowIVar): String = colorNameOfVar(rivar.id, rivar.toString)

    private def colorNameOfVar(id: Int, string: String) =
      val color = ColorArray(id % ColorArray.length)
      paint(color, string)

    def colorTypeLike(obj: Any): String =
      val toStr = obj match
        case o: List[_] => o.mkString(", ")
        case o => o.toString
      colorIvarsIn(toStr)

    def colorIvarsIn(str: String): String =
      val rivarMatcher = "'[A-Za-z]+(\\d+)".r
      rivarMatcher.replaceAllIn(str, { m =>
        val id = m.group(1).toInt
        colorNameOfVar(id, m.matched)
      })
  }

  object Coloring {
    private val ColorArray = Array(
      // AnsiColor.BLACK,
      AnsiColor.RED,
      AnsiColor.GREEN,
      AnsiColor.YELLOW,
      AnsiColor.BLUE,
      AnsiColor.MAGENTA,
      AnsiColor.CYAN,
      AnsiColor.WHITE
      )


    val Disabled = new Coloring(false)
    val Enabled = new Coloring(true)

    def apply(useColors: Boolean): Coloring = if useColors then Enabled else Disabled
  }


  /** Strategy to trace type inference. */
  trait TypeInfLogger {

    // increase indent
    def startFrame(term: KExpr): Unit = {}

    def boundAdded(ivar: KInferenceVar, boundKind: BoundKind, bound: KStackTypeItem): Unit = {}
    def ivarInstantiated(ivar: KInferenceVar, inst: KDataType): Unit = {}

    def rivarInstantiated(rivar: KRowIVar, inst: List[KStackTypeItem]): Unit = {}

    def unificationRequest(left: StackType, right: StackType): Unit = {}

    def subUnificationRequest(left: List[KStackTypeItem], right: List[KStackTypeItem]): Unit = {}

    def recordAliasing(r: KRowIVar, s: KRowIVar, inst: List[KStackTypeItem]): Unit = {}


    // decrease indent
    def endFrameWithFinalType(st: StackType): Unit = {}
  }

  object NoopLogger extends TypeInfLogger

  class DebugTypeInfLogger(private val out: PrintStream,
                           private val colors: Coloring,
                           private var indent: Int) extends TypeInfLogger {

    private def println(any: Any): Unit = {
      out.print(any.toString.indent(indent))
    }

    override def startFrame(term: KExpr): Unit = {
      println(s"- Resolving ${term.toString}\n" + term.pos.longString)
      indent += 4
    }

    override def endFrameWithFinalType(st: StackType): Unit = {
      println(s"- Final type: ${colors.paintRed(st)}")
      indent -= 4
    }

    override def unificationRequest(stLeft: StackType, stRight: StackType): Unit =
      println(s"- Unifying ${colors.colorTypeLike(stLeft)} with ${colors.colorTypeLike(stRight)}")

    override def subUnificationRequest(left: List[KStackTypeItem], right: List[KStackTypeItem]): Unit =
      println(s"-+ Unifying ${colors.colorTypeLike(left)} with ${colors.colorTypeLike(right)}")

    override def recordAliasing(r: KRowIVar, s: KRowIVar, inst: List[KStackTypeItem]): Unit =
      println(s"-+ Aliasing ${colors.colorTypeLike(r)} and ${colors.colorTypeLike(s)}")

    override def boundAdded(ivar: KInferenceVar, boundKind: BoundKind, bound: KStackTypeItem): Unit =
      println(s"- New bound: ${colors.colorTypeLike(ivar)} ${boundKind.symbol} ${colors.colorTypeLike(bound)}")

    override def rivarInstantiated(rivar: KRowIVar, inst: List[KStackTypeItem]): Unit =
      val instToStr = colors.colorTypeLike(inst)
      println(s"- Ivar instantiated: ${colors.colorNameOfVar(rivar)} := $instToStr")

    override def ivarInstantiated(ivar: KInferenceVar, inst: KDataType): Unit =
      println(s"- Ivar instantiated: ${colors.colorTypeLike(ivar)} := ${colors.colorTypeLike(inst)}")

  }

  object DebugTypeInfLogger {
    def apply(out: PrintStream = System.err, useColors: Boolean = true): DebugTypeInfLogger =
      new DebugTypeInfLogger(out, colors = Coloring(useColors), indent = 0)
  }

}
