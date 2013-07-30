package dhg.util.math

object NumUtil {

  implicit class EnrichedInt(val self: Int) extends AnyVal {
    def pow(e: Int): Int = (1 until e).foldLeft(self)((z, _) => z * self)
    def **(e: Int): Int = pow(e)
    def pow(e: Double): Double = math.pow(self, e)
    def **(e: Double): Double = math.pow(self, e)
  }

  implicit class EnrichedDouble(val self: Double) extends AnyVal {
    def pow(e: Double): Double = math.pow(self, e)
    def **(e: Double): Double = math.pow(self, e)
  }

}
