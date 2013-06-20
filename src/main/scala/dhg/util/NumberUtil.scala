package dhg.util

/**
 * @author Dan Garrette (dhg@cs.utexas.edu)
 */
object NumberUtil {

  implicit class Enriched_Int(val self: Int) extends AnyVal {

    /**
     * Shorthand for a range from this Int to the max integer value.
     */
    def up: Range = self to Int.MaxValue
  }

}
