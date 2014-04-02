package dhg.util.math

object MathUtil {

  def logadd(alog: Double, blog: Double): Double = {
    if (alog == Double.NegativeInfinity)
      blog
    else if (blog == Double.NegativeInfinity)
      alog
    else if (alog > blog)
      alog + math.log1p(math.exp(blog - alog))
    else
      blog + math.log1p(math.exp(alog - blog))
  }

  implicit class DoubleWithExponentOp(val v: Double) extends AnyVal {
    def **(o: Int) = math.pow(v, o)
    def **(o: Double) = math.pow(v, o)
  }

  implicit class IntWithExponentOp(val v: Int) extends AnyVal {
    def **(o: Int) = math.pow(v, o)
    def **(o: Double) = math.pow(v, o)
  }

}
