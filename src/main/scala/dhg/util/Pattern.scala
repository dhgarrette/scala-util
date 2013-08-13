package dhg.util

import scala.collection.generic.CanBuildFrom

/**
 * Pattern-matching utilities
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object Pattern {

  object UInt {
    val IntRE = """^(-?\d+)$""".r
    def unapply(v: String): Option[Int] = v match {
      case IntRE(s) => Some(s.toInt)
      case _ => None
    }
  }
  //  implicit def int2unapplyInt(objA: Int.type) = UInt

  object UDouble {
    val DoubleRE = """^(-?\d+\.?\d*|-?\d*\.?\d+)$""".r
    def unapply(v: String): Option[Double] = v match {
      case DoubleRE(s) => Some(s.toDouble)
      case _ => None
    }
  }
  //  implicit def double2unapplyDouble(objA: Double.type) = UDouble

  object UBoolean {
    val booleanRE = """([Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee])""".r
    def unapply(v: String): Option[Boolean] = v match {
      case booleanRE(s) => Some(s.toBoolean)
      case _ => None
    }
  }

  object UMap {
    def unapplySeq[A, B](m: Map[A, B]): Option[Seq[(A, B)]] = Some(m.toIndexedSeq)
  }

  object USet {
    def unapplySeq[A](s: Set[A]): Option[Seq[A]] = Some(s.toIndexedSeq)
  }

  /**
   * Make it possible to do:
   *   val a -> b = (1,2)
   */
  object -> {
    def unapply[A, B](pair: (A, B)): Option[(A, B)] = Some(pair)
  }

  /**
   *
   */
  object RangeString {
    val RangeRE = """^(\d+)-(\d+)$""".r
    def apply(s: String): Seq[Int] = {
      s.replaceAll("\\s+", "").split(",").flatMap {
        case UInt(i) => i to i
        case RangeRE(UInt(b), UInt(e)) if b <= e => b to e
      }
    }

    /**
     * Make a succinct string that describes the given sequence.
     */
    def apply(seq: Seq[Int]): String = {
      assert(seq.nonEmpty, "cannot make empty sequence into a range string")
      (-2 +: seq).sliding(2).foldLeft(Vector[Vector[Int]]()) {
        case (_, Seq(_, b)) if b < 0 =>
          throw new AssertionError(s"negative numbers are not permitted: $seq")
        case ((z :+ c), Seq(a, b)) =>
          if (a != b - 1)
            (z :+ c) :+ Vector(b)
          else
            (z :+ (c :+ b))
        case (z, Seq(a, b)) =>
          z :+ Vector(b)
      }
        .map {
          case Seq(x) => x.toString
          case s => s.head + "-" + s.last
        }.mkString(",")
    }

    def unapply(s: String): Option[Seq[Int]] = Some(apply(s))
    def unapply(seq: Seq[Int]): Option[String] = Some(apply(seq))
  }

  class RangeString(max: Int) {
    val OpenRangeRE = """^(\d+)-$""".r
    def apply(s: String): Seq[Int] = {
      s.replaceAll("\\s+", "").split(",").flatMap {
        case OpenRangeRE(UInt(b)) => b to max
        case UInt(i) => i to i
        case RangeString.RangeRE(UInt(b), UInt(e)) if b <= e => b to e
      }
    }
    def unapply(s: String): Option[Seq[Int]] = Some(apply(s))
  }

  object Iterable {
    def unapplySeq[T](s: Iterable[T]): Option[Seq[T]] =
      Some(s.toIndexedSeq)
  }

}
