package dhg.util

/**
 * String Utilities
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
      val parts = self.split(str)
      val (front, back) = parts.splitAt(parts.size - n + 1)
      if (front.nonEmpty)
        front.mkString(str) +: back
      else
        back
    }

    /**
     * Add newlines to a string such that each line is `width` or less.  Line
     * splits happen only on whitespace.  In the case where a single 'word'
     * is longer than `width`, the newline will simply be inserted after the
     * word.
     */
    def wrap(width: Int = 80): String = {
      val lines =
        self.split("\n").flatMap { line =>
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
      lines.mkString("\n")
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
}
