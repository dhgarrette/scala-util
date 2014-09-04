package dhg.util.math

import dhg.util.CollectionUtil._
import dhg.util.Time._
import dhg.util.FileUtil._
import dhg.util.StringUtil._
import scala.annotation.tailrec
import scala.math.{ pow, exp, log }
import org.apache.commons.math3.random.{ RandomGenerator }

object FastMathUtil {

  def sum(a: Array[Double], length: Int): Double = {
    assert(length <= a.length, s"Passed in a length of $length to sum, for an array of length ${a.length}")
    var accum = 0.0
    var i = 0
    while (i < length) {
      accum += a(i)
      i += 1
    }
    accum
  }

  def activeSum(a: Array[Double], active: Array[Int], activeCount: Int): Double = {
    assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to activeSum, for an active array of length ${active.length}")
    assert(activeCount <= a.length, s"Passed in an activeCount of $activeCount to activeSum, for an array of length ${a.length}")
    var accum = 0.0
    var i = 0
    while (i < activeCount) {
      accum += a(active(i))
      i += 1
    }
    accum
  }

  /**
   * Sums together things in log space.
   * @return log(exp(a) + exp(b))
   *
   * stolen from breeze
   */
  def logSum(a: Double, b: Double) = {
    if (a.isNegInfinity) b
    else if (b.isNegInfinity) a
    else if (a < b) b + scala.math.log1p(exp(a - b))
    else a + scala.math.log1p(exp(b - a))
  }

  /**
   * Sums together things in log space.
   * @return log(\sum exp(a_i))
   *
   * stolen from breeze
   */
  def logSum(a: Double, b: Double, c: Double*): Double = {
    logSum(logSum(a, b) +: c)
  }

  /**
   * Sums together things in log space.
   * @return log(\sum exp(a_i))
   *
   * stolen from breeze
   */
  def logSum(iter: Iterator[Double], max: Double): Double = {
    require(iter.hasNext)
    if (max.isInfinite) {
      max
    }
    else {
      val aux = (0.0 /: iter) {
        (acc, x) => if (x.isNegInfinity) acc else acc + exp(x - max)
      }
      if (aux != 0)
        max + scala.math.log(aux)
      else
        max
    }
  }

  /**
   * Sums together things in log space.
   * @return log(\sum exp(a_i))
   *
   * stolen from breeze
   */
  def logSum(a: Seq[Double]): Double = {
    a.length match {
      case 0 => Double.NegativeInfinity
      case 1 => a(0)
      case 2 => logSum(a(0), a(1))
      case _ => logSum(a.iterator, a reduceLeft (_ max _))
    }
  }

  /**
   * Sums together the first length elements in log space.
   * The length parameter is used to make things faster.
   *
   * This method needs to be fast. Don't scala-ify it.
   * 
   * log(\sum^length exp(a_i))
   *
   * stolen from breeze
   */
  def logSum(a: Array[Double], length: Int): Double = {
    assert(length <= a.length, s"Passed in a length of $length to logSum, for an array of length ${a.length}")
    if (length == 0) Double.NegativeInfinity
    else if (length == 1) a(0)
    else if (length == 2) logSum(a(0), a(1))
    else {
      val m = max(a, length)
      if (m.isInfinite) m // avoids NaN issue
      else {
        var i = 0
        var accum = 0.0
        while (i < length) {
          accum += scala.math.exp(a(i) - m)
          i += 1
        }
        m + scala.math.log(accum)
      }
    }
  }

  def activeLogSum(a: Array[Double], active: Array[Int], activeCount: Int): Double = {
    assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to activeLogSum, for an active array of length ${active.length}")
    assert(activeCount <= a.length, s"Passed in an activeCount of $activeCount to activeLogSum, for an array of length ${a.length}")

    if (activeCount == 0) Double.NegativeInfinity
    else if (activeCount == 1) a(active(0))
    else if (activeCount == 2) logSum(a(active(0)), a(active(1)))
    else {
      val m = activeMax(a, active, activeCount)
      if (m.isInfinite) m // avoids NaN issue
      else {
        var i = 0
        var accum = 0.0
        while (i < activeCount) {
          accum += scala.math.exp(a(active(i)) - m)
          i += 1
        }
        m + scala.math.log(accum)
      }
    }
  }

  /**
   * fast versions of max. Useful for the fast logsum.
   *
   * stolen from breeze
   */
  def max(a: Array[Double], length: Int) = {
    assert(length != 0, s"Cannot compute max for a length of zero. (Array has length ${a.length})")
    assert(length <= a.length, s"Passed in a length of $length to max, for an array of length ${a.length}")

    var i = 1
    var max = a(0)
    while (i < length) {
      if (a(i) > max) max = a(i)
      i += 1
    }
    max
  }

  /**
   *
   */
  def activeMax(a: Array[Double], active: Array[Int], activeCount: Int) = {
    assert(activeCount != 0, s"Cannot compute activeMax for an activeCount of zero. (Active array has length ${active.length})")
    assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to activeMax, for an active array of length ${active.length}")
    assert(activeCount <= a.length, s"Passed in an activeCount of $activeCount to activeMax, for an array of length ${a.length}")

    var max = a(active(0))
    var i = 1
    while (i < activeCount) {
      if (a(active(i)) > max)
        max = a(active(i))
      i += 1
    }
    max
  }

  /**
   *
   */
  def argmax(a: Array[Double], length: Int) = {
    assert(length != 0, s"Cannot compute argmax for a length of zero. (Array has length ${a.length})")
    assert(length <= a.length, s"Passed in a length of $length to argmax, for an array of length ${a.length}")

    var max = a(0)
    var maxIdx = 0
    var i = 1
    while (i < length) {
      if (a(i) > max) {
        max = a(i)
        maxIdx = i
      }
      i += 1
    }
    maxIdx
  }

  /**
   *
   */
  def activeArgmax(a: Array[Double], active: Array[Int], activeCount: Int) = {
    assert(activeCount != 0, s"Cannot compute activeArgmax for an activeCount of zero. (Active array has length ${active.length})")
    assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to activeArgmax, for an active array of length ${active.length}")
    assert(activeCount <= a.length, s"Passed in an activeCount of $activeCount to activeArgmax, for an array of length ${a.length}")

    var maxIdx = active(0)
    var max = a(maxIdx)
    var i = 1
    while (i < activeCount) {
      if (a(active(i)) > max) {
        maxIdx = active(i)
        max = a(maxIdx)
      }
      i += 1
    }
    maxIdx
  }

  /**
   * sample from the distribution
   */
  def choose(dist: Array[Double], count: Int, rand: RandomGenerator): Int = {
    assert(count != 0, s"Cannot choose for a count of zero. (Array has length ${dist.length})")
    assert(count <= dist.length, s"Passed in a count of $count to choose, for an array of length ${dist.length}")

    if (count == 1) return 0
    val r = rand.nextDouble
    val pSum = sum(dist, count)
    var s = pSum * r
    var i = 0
    while (i < count) {
      s -= dist(i)
      if (s < 0) return i
      i += 1
    }
    assert(!pSum.isInfinite, f"in choose, pSum=$pSum")
    sys.error(f"No value chosen!  ${dist.mkString("[", ", ", "]")}, r=$r%.2f")
  }

  /**
   * Don't let all active values to be log(0)
   */
  def activeLogChoose(logDist: Array[Double], active: Array[Int], activeCount: Int, rand: RandomGenerator): Int = {
    assert(activeCount != 0, s"Cannot activeLogChoose for an activeCount of zero. (Active array has length ${active.length})")
    assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to activeLogChoose, for an active array of length ${active.length}")
    assert(activeCount <= logDist.length, s"Passed in an activeCount of $activeCount to activeLogChoose, for a logDist array of length ${logDist.length}")

    if (activeCount == 1) return active(0)
    val logSum = activeLogSum(logDist, active, activeCount)
    var prob = rand.nextDouble
    var i = 0
    while (i < activeCount) {
      val ai = active(i)
      prob -= exp(logDist(ai) - logSum)
      if (prob < 0) return ai
      i += 1
    }
    sys.error(f"No value chosen!  ${logDist.mkString("[", ", ", "]")}, ${active.take(activeCount).mkString("[[", ", ", "]]")}${active.drop(activeCount).mkString("", " ", "]")}")
  }

  /**
   * In-place normalization of elements up to length
   */
  def normalize(a: Array[Double], length: Int): Unit = {
    assert(length != 0, s"Cannot normalize for a length of zero. (Array has length ${a.length})")
    // length <= a.length
    var s = sum(a, length)
    var i = 0
    while (i < length) {
      a(i) /= s
      i += 1
    }
  }

  /**
   * In-place normalization of active elements
   */
  def activeNormalize(a: Array[Double], active: Array[Int], activeCount: Int): Unit = {
    assert(activeCount != 0, s"Cannot activeNormalize for an activeCount of zero. (Active array has length ${active.length})")
    // activeCount <= active.length
    // activeCount <= a.length
    var s = activeSum(a, active, activeCount)
    var i = 0
    while (i < activeCount) {
      val ai = active(i)
      a(ai) /= s
      i += 1
    }
  }

  /**
   * In-place normalization, then logging, of elements up to length
   */
  def normalizeAndLog(a: Array[Double], length: Int): Unit = {
    assert(length != 0, s"Cannot normalizeAndLog for a length of zero. (Array has length ${a.length})")
    // length <= a.length
    var s = sum(a, length)
    var i = 0
    while (i < length) {
      a(i) = log(a(i) / s)
      i += 1
    }
  }

  /**
   * In-place normalization, then logging, of active elements
   */
  def activeNormalizeAndLog(a: Array[Double], active: Array[Int], activeCount: Int): Unit = {
    assert(activeCount != 0, s"Cannot activeNormalizeAndLog for an activeCount of zero. (Active array has length ${active.length})")
    // activeCount <= active.length
    // activeCount <= a.length
    var s = activeSum(a, active, activeCount)
    var i = 0
    while (i < activeCount) {
      val ai = active(i)
      a(ai) = log(a(ai) / s)
      i += 1
    }
  }

  /**
   * In-place normalization of log-valued elements up to length
   */
  def logNormalize(logData: Array[Double], length: Int): Unit = {
    assert(length != 0, s"Cannot logNormalize for a length of zero. (Array has length ${logData.length})")
    // length <= a.length
    var logsum = logSum(logData, length)
    var i = 0
    while (i < length) {
      logData(i) = logData(i) - logsum
      i += 1
    }
  }

  /**
   * In-place normalization of active log-valued elements
   */
  def activeLogNormalize(logData: Array[Double], active: Array[Int], activeCount: Int): Unit = {
    assert(activeCount != 0, s"Cannot activeLogNormalize for an activeCount of zero. (Active array has length ${active.length})")
    // activeCount <= active.length
    // activeCount <= a.length
    var logSum = activeLogSum(logData, active, activeCount)
    var i = 0
    while (i < activeCount) {
      val ai = active(i)
      logData(ai) = logData(ai) - logSum
      i += 1
    }
  }

  /**
   * makes log-normalized probabilities
   */
  def convertToLogDirichletDraw(counts: Array[Double], length: Int, rand: RandomGenerator): Unit = {
    assert(length != 0, s"Cannot convertToLogDirichletDraw for a length of zero. (Array has length ${counts.length})")
    assert(length <= counts.length, s"Passed in a length of $length to convertToLogDirichletDraw, for an array of length ${counts.length}")

    var i = 0
    while (i < length) {
      val gd = gammaLogDraw(counts(i), rand)
      counts(i) = gd
      i += 1
    }
    logNormalize(counts, length)
  }

  /**
   * makes log-normalized probabilities
   */
  def convertActiveToLogDirichletDraw(counts: Array[Double], active: Array[Int], activeCount: Int, rand: RandomGenerator): Unit = {
    assert(activeCount != 0, s"Cannot convertActiveToLogDirichletDraw for an activeCount of zero. (Active array has length ${active.length})")
    assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to convertActiveToLogDirichletDraw, for an active array of length ${active.length}")
    assert(activeCount <= counts.length, s"Passed in an activeCount of $activeCount to convertActiveToLogDirichletDraw, for an array of length ${counts.length}")

    var i = 0
    while (i < activeCount) {
      val ai = active(i)
      val gd = gammaLogDraw(counts(ai), rand)
      counts(ai) = gd
      i += 1
    }
    activeLogNormalize(counts, active, activeCount)
  }

  def gammaLogDraw(shape: Double, rand: RandomGenerator): Double = {
    if (shape < 1) {
      // adapted from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
      @tailrec
      def rec: Double = {
        val u = rand.nextDouble
        val v = -math.log(rand.nextDouble)
        val logU = log(u)
        if (logU <= math.log1p(-shape)) {
          val logV = log(v)
          val logX = logU / shape
          if (logX <= logV) logX
          else rec
        }
        else {
          val y = -log((1 - u) / shape)
          val logX = math.log(1.0 - shape + shape * y) / shape
          if (logX <= math.log(v + y)) logX
          else rec
        }
      }
      rec
    }
    else math.log(gammaDraw(shape, rand))
  }

  def gammaDraw(shape: Double, rand: RandomGenerator): Double = {
    if (shape == 1.0) {
      -math.log(rand.nextDouble)
    }
    else if (shape < 1.0) {
      // from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
      @tailrec
      def rec: Double = {
        val u = rand.nextDouble
        val v = -math.log(rand.nextDouble)
        if (u <= 1.0 - shape) {
          val x = pow(u, 1.0 / shape)
          if (x <= v) x
          else rec
        }
        else {
          val y = -log((1 - u) / shape)
          val x = pow(1.0 - shape + shape * y, 1.0 / shape)
          if (x <= (v + y)) x
          else rec
        }
      }
      rec
    }
    else {
      // from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
      val d = shape - 1.0 / 3.0
      val c = 1.0 / math.sqrt(9.0 * d)
      var r = 0.0
      var ok = false
      while (!ok) {
        var v = 0.0
        var x = 0.0
        do {
          x = rand.nextGaussian
          v = 1.0 + c * x
        } while (v <= 0)

        v = v * v * v
        val x2 = x * x
        val u = rand.nextDouble
        if (u < 1.0 - 0.0331 * (x2 * x2)
          || log(u) < 0.5 * x2 + d * (1.0 - v + log(v))) {
          r = d * v
          ok = true
        }
      }
      r
    }
  }

}
