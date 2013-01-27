package dhg.util

import scala.math._

/**
 * This Numeric class represents values using logarithms.  The underlying
 * logarithmic representation is completely hidden from the calling code.
 *
 * This class exists to allow for the use of obvious operators (* for
 * multiplication instead of + on logarithms) and to prevent coding mistakes
 * resulting from the inadvertent mixing of logarithmic and non-logarithmic
 * Double representations of probabilities.  Additionally, it is possible to
 * use the `sum` and `product` collection methods on collections of
 * Probabilities, and get the expected results.
 *
 * All to* methods return the (non-logarithmic) value stored.  The only
 * way to access the actual logarithmic value is by the 'logValue' field.
 */
class LogNum(val logValue: Double) extends AnyVal with Ordered[LogNum] {
  def +(other: LogNum): LogNum = {
    val oLogValue = other.logValue
    if (logValue == Double.NegativeInfinity)
      other
    else if (oLogValue == Double.NegativeInfinity)
      this
    else if (logValue > oLogValue)
      new LogNum(logValue + log1p(exp(oLogValue - logValue)))
    else
      new LogNum(oLogValue + log1p(exp(logValue - oLogValue)))
  }

  def -(other: LogNum): LogNum = {
    val oLogValue = other.logValue
    if (oLogValue == 0.0)
      this
    else if (logValue < oLogValue)
      sys.error("subtraction results in a negative LogNum")
    else
      new LogNum(logValue + log1p(-exp(oLogValue - logValue)))
  }

  def *(other: LogNum): LogNum = new LogNum(logValue + other.logValue)
  def /(other: LogNum): LogNum = new LogNum(logValue - other.logValue)
  def **(pow: LogNum): LogNum = new LogNum(pow.toDouble * logValue)

  override def compare(that: LogNum) = logValue.compare(that.logValue)
  def max(that: LogNum): LogNum = if (this.logValue > that.logValue) this else that
  def min(that: LogNum): LogNum = if (this.logValue < that.logValue) this else that

  def approx(o: LogNum, tolerance: Double): Boolean = (logValue - o.logValue).abs < tolerance
  def approx(o: LogNum): Boolean = this.approx(o, 0.00000001)

  def toInt = toDouble.toInt
  def toLong = toDouble.toLong
  def toFloat = toDouble.toFloat
  def toDouble = exp(logValue)

  override def toString = s"LogNum(${toDouble})"
}

object LogNum {

  def apply[N](n: N)(implicit num: Numeric[N]) = {
    n match {
      case logNum: LogNum => logNum
      case _ => new LogNum(log(num.toDouble(n)))
    }
  }

  val zero = new LogNum(Double.NegativeInfinity)
  val one = new LogNum(0.0)

  trait LogNumOrdering extends Ordering[LogNum] {
    override def compare(a: LogNum, b: LogNum) = a compare b
  }

  implicit object LogNumIsFractional extends LogNumIsFractional with LogNumOrdering

  trait LogNumIsFractional extends Fractional[LogNum] {
    def plus(x: LogNum, y: LogNum): LogNum = x + y
    def minus(x: LogNum, y: LogNum): LogNum = x - y
    def times(x: LogNum, y: LogNum): LogNum = x * y
    def div(x: LogNum, y: LogNum): LogNum = x / y
    def negate(x: LogNum): LogNum = sys.error("LogNum values cannot be negated")
    def fromInt(x: Int): LogNum = new LogNum(log(x))
    def toInt(x: LogNum): Int = x.toInt
    def toLong(x: LogNum): Long = x.toLong
    def toFloat(x: LogNum): Float = x.toFloat
    def toDouble(x: LogNum): Double = x.toDouble
    override def zero = LogNum.zero
    override def one = LogNum.one
  }

  implicit class EnrichedNumeric[N](self: N)(implicit num: Numeric[N]) {
    def toLogNum = new LogNum(log(num.toDouble(self)))
  }

}
