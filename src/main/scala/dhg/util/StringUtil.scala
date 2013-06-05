package dhg.util

object StringUtil {

  val WhitespaceRe = """\s*""".r
  val RTrimRe = """(.*\S)\s*""".r
  implicit class EnrichedString(val self: String) extends AnyVal {
    def rtrim = self match {
      case WhitespaceRe() => ""
      case RTrimRe(trimmed) => trimmed
    }

    def splitlines = self.split("\n")
    def splitWhitespace = self.split("\\s+")

    def rsplit(str: String, n: Int) = {
      val parts = self.split(str)
      val (front, back) = parts.splitAt(parts.size - n + 1)
      if (front.nonEmpty)
        front.mkString(str) +: back
      else
        back
    }

    def wrap(width: Int = 80) = {
      val (completeLines, lastLine) =
        self.split("\\s+").foldLeft((Vector[String](), "")) {
          case ((lines, currLine), tok) =>
            if (currLine.size + tok.size + 1 > width)
              (lines :+ currLine, tok)
            else if (currLine.isEmpty)
              (lines, tok)
            else
              (lines, currLine + " " + tok)
        }
      val lines = completeLines :+ lastLine
      //lines.map(s => f"$s%-80s|").mkString("\n")
      lines.mkString("\n")
    }

  }
}
