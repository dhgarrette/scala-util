package dhg.util

import scala.annotation.tailrec
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.SeqLike
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.Builder
import scala.util.Random

/**
 * @author Dan Garrette (dhg@cs.utexas.edu)
 */
object NumberUtil {

  implicit class Enriched_Int(val self: Int) extends AnyVal {
    def up: Range = self to Int.MaxValue
  }

}
