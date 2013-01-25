package dhg.util

import org.junit.Assert._

object TestUtil {

  def assertEqualsDouble(expected: Double, actual: Double) {
    assertEquals(expected, actual, 0.000000001)
  }

  def assertEqualsDoubleMap[T](expected: Map[T, Double], actual: Map[T, Double]) {
    assertEquals("Wrong size.", expected.size, actual.size)
    def keystr(m: Map[T, Double]) = s"${m.keys.toVector.map(_.toString).sorted.mkString(", ")}"
    assertEquals("Wrong keys.", keystr(expected), keystr(actual))
    for ((k, ev) <- expected)
      assertEqualsDouble(ev, actual(k))
  }

}
