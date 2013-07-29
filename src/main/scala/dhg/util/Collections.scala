package dhg.util

import dhg.util.CollectionUtil._
import scala.collection.GenTraversableOnce
import scala.collection.mutable

/**
 * Collections
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object Collections {

  /**
   * A set for which `contains` always returns `true`.
   */
  class UniversalSet[A] extends Set[A] {
    override def contains(key: A): Boolean = true
    override def iterator: Iterator[A] = throw new NotImplementedError("UniversalSet cannot be iterated over")
    override def +(elem: A): UniversalSet[A] = this // Anything added to a UniversalSet is still a UniversalSet
    override def -(elem: A): UniversalSet[A] = throw new NotImplementedError("Nothing can be removed from a UniversalSet")
    override def toString() = "UniversalSet()"
  }
  object UniversalSet {
    def apply[A]() = new UniversalSet[A]
  }

  /**
   * An Iterator-ish class that returns a vector of next items while the
   * condition is met, but updates the underlying iterator correctly so that
   * the method can be called repeatedly to get subsequent elements.
   */
  class NextWhileIteratorish[A](stuff: Iterator[A]) {
    private[this] var itr = stuff
    def nextWhile(p: A => Boolean): Vector[A] = {
      if (itr.hasNext) {
        var a = itr.next()
        if (p(a)) {
          a +: nextWhile(p)
        }
        else {
          itr = a +: itr
          Vector()
        }
      }
      else {
        Vector()
      }
    }
  }

  //
  //
  //

  /**
   * A `Map` implementation that generates values for a `default` function
   * when keys are requested, but that remembers the calculated value for
   * for future requests
   */
  class MemoMap[A, B](startEntries: Map[A, B], default: A => B) extends (A => B) with Iterable[(A, B)] { //mutable.Map[A, B] {
    private[this] val cache = mutable.Map[A, B]() ++ startEntries
    override def apply(key: A): B =
      synchronized {
        cache.getOrElseUpdate(key, default(key))
      }
    override def size = cache.size
    override def iterator: Iterator[(A, B)] = cache.iterator
  }

  //
  //
  //

  /**
   * A list that drops elements off the tail when the length is exceeded.
   * Also allows for skipping elements during iteration.
   */
  class History[T] private (length: Int, lag: Int) {
    private[this] val q = scala.collection.mutable.Queue[T]()
    def ::=(t: T) = { q.enqueue(t); if (q.length > length) q.dequeue(); this }
    def :::=(ts: GenTraversableOnce[T]) = { for (t <- ts) this ::= t; this }
    def head = q.last
    def iterator = q.reverseIterator.grouped(lag + 1).map(_(0))
  }
  object History {
    def apply[T](length: Int, lag: Int): History[T] = new History[T](if (length > 0) length else 1, lag)
    def apply[T](length: Int, e: T, lag: Int): History[T] = History(length, lag) ::= e
  }

}
