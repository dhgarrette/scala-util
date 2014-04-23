package dhg.util.math

object MathUtil {

  def logadd(alog: Double, blog: Double): Double = {
    if (alog.isNegInfinity)
      blog
    else if (blog.isNegInfinity)
      alog
    else if (alog > blog)
      alog + math.log1p(math.exp(blog - alog))
    else
      blog + math.log1p(math.exp(alog - blog))
  }

}
