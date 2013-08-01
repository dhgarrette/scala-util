package dhg.util

/**
 * Enhancement methods for numbers
 * 
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object NumberUtil {

  implicit class Enriched_Int(val self: Int) extends AnyVal {

    /**
     * Shorthand for a range from this Int to the max integer value.
     */
    def up: Range = self to Int.MaxValue
    def upi: Iterator[Int] = Iterator.from(self)
  }

}
