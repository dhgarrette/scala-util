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
  }

}
