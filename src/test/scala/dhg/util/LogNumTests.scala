package dhg.util

import scala.math._

import org.junit.Assert._
import org.junit.Test

import dhg.util.LogNum._

class LogNumTests {

  @Test
  def test_sum() {
    val a = new LogNum(-772.740105887932)
    val l = List(a)
    val s = LogNum.zero + a
    assertTrue(s != LogNum.zero)
  }

  @Test
  def test_operators() {
    val a: LogNum = 3.toLogNum
    val b: LogNum = 5.toLogNum
    val c: LogNum = 6.toLogNum
    val d: LogNum = 2.toLogNum
    val e: LogNum = 8.toLogNum

    assertEquals(log(3.0), (a.logValue: Double), 0.0000001)
    assertEquals(3.0, (a.toDouble: Double), 0.0000001)
    assertEquals(3, (a.toInt: Int), 0.0000001)

    assertEqualsLog(b, d + a)
    assertEqualsLog(d, b - a)
    assertEqualsLog(c, d * a)
    assertEqualsLog(d, c / a)
    assertEqualsLog(e, d ** a)

    assertEqualsLog(b, a max b)
    assertEqualsLog(b, b max a)
    assertEqualsLog(a, a min b)
    assertEqualsLog(a, b min a)

    assertTrue(a < b)
    assertFalse(a > b)
    assertTrue(b > a)
    assertFalse(b < a)

    assertEqualsLog(LogNum(10), List(a, b, d).sum)
    assertEqualsNumericLog(b, List(a, b).max)
    assertEqualsNumericLog(b, List(b, a).max)
    assertEqualsNumericLog(a, List(a, b).min)
    assertEqualsNumericLog(a, List(b, a).min)
  }

  @Test
  def test_numeric() {
    val a: LogNum = 3.toLogNum
    val b: LogNum = 5.toLogNum
    val c: LogNum = 6.toLogNum
    val d: LogNum = 2.toLogNum
    val e: LogNum = 8.toLogNum

    def stuff[N](a: N, b: N, c: N, d: N, e: N)(implicit num: Fractional[N]) = {
      assertEqualsNumericLog(b, num.plus(d, a))
      assertEqualsNumericLog(d, num.minus(b, a))
      assertEqualsNumericLog(c, num.times(d, a))
      assertEqualsNumericLog(d, num.div(c, a))

      assertEqualsNumericLog(b, List(a, b).max)
      assertEqualsNumericLog(b, List(b, a).max)
      assertEqualsNumericLog(a, List(a, b).min)
      assertEqualsNumericLog(a, List(b, a).min)
    }

    stuff(a, b, c, d, e)
  }

  def assertEqualsLog(a: LogNum, b: LogNum) {
    assertEquals(a.toDouble, b.toDouble, 0.0000001)
  }

  def assertEqualsNumericLog[N](a: N, b: N)(implicit num: Numeric[N]) {
    (a, b) match {
      case (a: LogNum, b: LogNum) => assertEqualsLog(a, b)
      case _ => fail(s"a and b are not LogNum instances: a:[$a], b:[$b]")
    }
  }

}
