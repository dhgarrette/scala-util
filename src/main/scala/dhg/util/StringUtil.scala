package dhg.util

/**
 * Enhancement methods for Strings
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object StringUtil {

  val WhitespaceRe = """\s*""".r
  val RTrimRe = """(.*\S)\s*""".r

  implicit class EnrichedString(val self: String) extends AnyVal {

    /**
     * Trim whitespace only from the right side of the string
     */
    def rtrim = self match {
      case WhitespaceRe() => ""
      case RTrimRe(trimmed) => trimmed
    }

    /**
     * Split on newlines
     */
    def splitlines: Array[String] = self.split("\n")

    /**
     * Split on whitespace
     */
    def splitWhitespace: Array[String] = self.split("\\s+")

    /**
     * Split a string into `n` pieces, starting from the right side.
     */
    def rsplit(str: String, n: Int): Array[String] = {
      self.reverse.split(str, n).map(_.reverse).reverse
    }

    /**
     * Add newlines to a string such that each line is `width` or less.  Line
     * splits happen only on whitespace.  In the case where a single 'word'
     * is longer than `width`, the newline will simply be inserted after the
     * word.
     */
    def wrapToLines(width: Int = 80): Vector[String] = {
      self.split("\n").toVector.flatMap { line =>
        val (completeLines, lastLine) =
          line.split("\\s+").foldLeft((Vector[String](), "")) {
            case ((lines, currLine), tok) =>
              if (currLine.size + tok.size + 1 > width)
                (lines :+ currLine, tok)
              else if (currLine.isEmpty)
                (lines, tok)
              else
                (lines, currLine + " " + tok)
          }
        completeLines :+ lastLine
      }
      //lines.map(s => f"$s%-80s|").mkString("\n")
    }

    /**
     * Add newlines to a string such that each line is `width` or less.  Line
     * splits happen only on whitespace.  In the case where a single 'word'
     * is longer than `width`, the newline will simply be inserted after the
     * word.
     */
    def wrap(width: Int = 80): String = {
      this.wrapToLines(width).mkString("\n")
    }

    /**
     * Indent the left side of each line by the given number of spaces.
     */
    def indent(spaces: Int): String = {
      indent(" " * spaces)
    }

    /**
     * Append the given string to the left side of each line.
     */
    def indent(leftColumn: String): String = {
      self.split("\n").map(leftColumn + _).mkString("\n")
    }

  }

  def sideBySideStrings(spaceBuffer: Int, columns: String*) = {
    sideBySide(spaceBuffer, columns.map(_.split("\n").toVector): _*).mkString("\n")
  }

  def sideBySide(spaceBuffer: Int, columns: Vector[String]*) = {
    val maxHeight = columns.map(_.size).max
    val vertBuffered =
      for (c <- columns) yield {
        c ++ Vector.fill(maxHeight - c.size)("")
      }
    val horizBuffered =
      (for (c <- vertBuffered.dropRight(1)) yield {
        val maxLineLength = c.map(_.length).max
        for (line <- c) yield {
          line + (" " * (maxLineLength - line.length))
        }
      }) :+ vertBuffered.last
    for (columnLines <- horizBuffered.transpose) yield {
      columnLines.mkString(" " * spaceBuffer)
    }
  }

}
