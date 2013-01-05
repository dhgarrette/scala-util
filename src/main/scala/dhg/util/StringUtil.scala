package dhg.util

object StringUtil {

  val RTrimRe = """(.*\S)\s*""".r
  implicit class EnrichedString(self: String) {
    def rtrim = self match {
      case "" => ""
      case RTrimRe(trimmed) => trimmed
    }
  }

}
