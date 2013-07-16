package dhg.util

import dhg.util.CollectionUtil._
import scala.util.matching.Regex

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
     * Split a string into `limit` pieces, starting from the left side.
     */
    def lsplit(str: String): Vector[String] = {
      new RegexMatcherSplitIterator(self, str)
        .toVector
        .dropRightWhile { case (b, e) => b == e && b > 0 }
        .map { case (b, e) => self.substring(b, e) }
    }

    /**
     * Split a string into `limit` pieces, starting from the left side.
     */
    def lsplit(str: String, limit: Int): Vector[String] = {
      val allSpans = new RegexMatcherSplitIterator(self, str).take(limit).toVector
      val leftSpans :+ h = allSpans
      val spans = leftSpans :+ (h._1 -> self.length())
      spans.map { case (b, e) => self.substring(b, e) }
    }

    /**
     * Split a string into `limit` pieces, starting from the right side.
     */
    def rsplit(str: String): Vector[String] = {
      new RegexMatcherSplitIterator(self, str)
        .toVector
        .dropWhile { case (b, e) => b == e }
        .map { case (b, e) => self.substring(b, e) }
    }

    /**
     * Split a string into `limit` pieces, starting from the right side.
     */
    def rsplit(str: String, limit: Int): Vector[String] = {
      val allSpans = new RegexMatcherSplitIterator(self, str).toVector
      val spans =
        if (allSpans.size > limit) {
          val h +: rightSpans = allSpans.takeRight(limit)
          (0 -> h._2) +: rightSpans
        }
        else
          allSpans
      spans.map { case (b, e) => self.substring(b, e) }
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

  private class RegexMatcherSplitIterator(str: String, pattern: String) extends Iterator[(Int, Int)] {
    val m = pattern.r.pattern.matcher(str)
    var prevE: Int = 0
    var queued: Option[(Int, Int)] = None
    var nextE: Option[Int] = Some(0)

    def hasNext() =
      if (queued.isDefined) {
        true
      }
      else if (m.find()) {
        queued = Some(prevE -> m.start)
        nextE = Some(m.end)
        true
      }
      else if (nextE.isDefined) {
        queued = Some(nextE.get -> str.length())
        nextE = None
        true
      }
      else {
        false
      }

    def next() =
      if (hasNext) {
        val n = queued.get
        prevE = nextE.getOrElse(-1)
        queued = None
        n
      }
      else
        Iterator().next()

    override def toString = s"RegexMatcherSplitIterator(prevE=$prevE, queued=$queued, nextE=$nextE)"
  }

  implicit class EnrichedRegex(val self: Regex) extends AnyVal {
	  def matches(s: String): Boolean = {
	    self.pattern.matcher(s).matches
	  }
  }

}
