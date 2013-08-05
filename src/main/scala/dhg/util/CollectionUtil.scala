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
import scala.collection.immutable.BitSet
import scala.collection.Parallelizable
import scala.collection.Parallel

/**
 * Enhancement methods for collections
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object CollectionUtil {

  //////////////////////////////////////////////////////
  // toTuple2: (T,T)
  // toTuple3: (T,T,T)
  // toTuple4: (T,T,T,T)
  // toTuple5: (T,T,T,T,T)
  //   - Convert this sequence to a tuple
  //////////////////////////////////////////////////////

  implicit class Enriched_toTuple_Seq[A](val seq: Seq[A]) extends AnyVal {
    def toTuple2 = seq match { case Seq(a, b) => (a, b); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple2: $x") }
    def toTuple3 = seq match { case Seq(a, b, c) => (a, b, c); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple3: $x") }
    def toTuple4 = seq match { case Seq(a, b, c, d) => (a, b, c, d); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple4: $x") }
    def toTuple5 = seq match { case Seq(a, b, c, d, e) => (a, b, c, d, e); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple5: $x") }
  }

  implicit class Enriched_toTuple_Array[A](val seq: Array[A]) extends AnyVal {
    def toTuple2 = seq match { case Array(a, b) => (a, b); case x => throw new AssertionError(s"Cannot convert array of length ${seq.size} into Tuple2: Array(${x.mkString(", ")})") }
    def toTuple3 = seq match { case Array(a, b, c) => (a, b, c); case x => throw new AssertionError(s"Cannot convert array of length ${seq.size} into Tuple3: Array(${x.mkString(", ")})") }
    def toTuple4 = seq match { case Array(a, b, c, d) => (a, b, c, d); case x => throw new AssertionError(s"Cannot convert array of length ${seq.size} into Tuple4: Array(${x.mkString(", ")})") }
    def toTuple5 = seq match { case Array(a, b, c, d, e) => (a, b, c, d, e); case x => throw new AssertionError(s"Cannot convert array of length ${seq.size} into Tuple5: Array(${x.mkString(", ")})") }
  }

  //////////////////////////////////////////////////////
  // +:(elem: B): Iterator[B]
  //   - Prepend an element to the iterator
  // :+(elem: B): Iterator[B]
  //   - Append an element to the end of the iterator
  //////////////////////////////////////////////////////

  implicit class Enriched_prependAppend_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Prepend an item to the front of the iterator
     *
     * @param elem	the item to be prepended
     * @return a new iterator
     */
    def +:[B >: A](elem: B): Iterator[B] =
      Iterator(elem) ++ self

    /**
     * Append an item to the end of the iterator
     *
     * @param elem	the item to be appended
     * @return a new iterator
     */
    def :+[B >: A](elem: B): Iterator[B] =
      self ++ Iterator(elem)
  }

  //////////////////////////////////////////////////////
  // counts(): Map[A, Int]
  //   - Map each distinct item in the collection to the number of times it appears.
  //////////////////////////////////////////////////////

  implicit class Enriched_counts_TraversableOnce[A](val self: TraversableOnce[A]) extends AnyVal {
    /**
     * Map each distinct item in the collection to the number of times it appears.
     *
     * @return Map from items to their counts
     */
    def counts(): Map[A, Int] = {
      val m = mutable.Map.empty[A, Int]
      for (item <- self) {
        val count = m.getOrElse(item, 0)
        m(item) = count + 1
      }
      m.toMap
    }
  }

  //////////////////////////////////////////////////////
  // groupBy(f: A => K): Repr[(R,U)]
  //   - Make Traversable.groupBy functionality available to Iterator
  //////////////////////////////////////////////////////

  implicit class Enriched_groupBy_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Same functionality as Traversable.groupBy(f)
     *
     * @param f	function mapping items to new keys
     * @return Map from new keys to original items
     */
    def groupBy[K](f: A => K): Map[K, Vector[A]] =
      this.groupBy(f, Vector.newBuilder[A])

    /**
     * Same functionality as Traversable.groupBy(f)
     *
     * @param f	function mapping items to new keys
     * @param builder	a builder to construct collections of items that have been grouped
     * @return Map from new keys to original items
     */
    def groupBy[K, That <: Iterable[A]](f: A => K, builder: => Builder[A, That]): Map[K, That] = {
      val m = mutable.Map.empty[K, Builder[A, That]]
      for (elem <- self) {
        val key = f(elem)
        val bldr = m.getOrElseUpdate(key, builder)
        bldr += elem
      }
      val b = Map.newBuilder[K, That]
      for ((k, v) <- m)
        b += ((k, v.result))
      b.result
    }
  }

  //////////////////////////////////////////////////////
  // groupByKey(): Map[T,Repr[U]]
  //   - For a collection of pairs (k,v), create a map from each `k` to the  
  //     collection of `v`s with which it is associated.
  //   - Equivalent to self.groupBy(_._1).map { case (k, elems) => (k, elems.map(_._2)) }
  //////////////////////////////////////////////////////

  implicit class Enriched_groupByKey_Traversable[K, V, Repr](val self: TraversableLike[(K, V), Repr]) extends AnyVal {
    /**
     * For a collection of pairs (k,v), create a map from each `k` to the
     * collection of `v`s with which it is associated.
     *
     * Equivalent to self.groupBy(_._1).map { case (k, elems) => (k, elems.map(_._2)) }
     *
     * @return Map from `k`s to collections of `v`s
     */
    def groupByKey[That](implicit bf: CanBuildFrom[Repr, V, That]): Map[K, That] = {
      val m = mutable.Map.empty[K, Builder[V, That]]
      for ((key, value) <- self) {
        val bldr = m.getOrElseUpdate(key, bf(self.asInstanceOf[Repr]))
        bldr += value
      }
      val b = immutable.Map.newBuilder[K, That]
      for ((k, v) <- m)
        b += ((k, v.result))
      b.result
    }
  }

  implicit class Enriched_groupByKey_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * For a collection of pairs, group by the first item in the pair.
     *
     * @return Map from first items of the pairs to collections of items that have been grouped
     */
    def groupByKey[T, U](implicit ev: A <:< (T, U)): Map[T, Vector[U]] =
      groupByKey(Vector.newBuilder[U])

    /**
     * For a collection of pairs, group by the first item in the pair.
     *
     * @param builder a builder to construct collections of items that have been grouped
     * @return Map from first items of the pairs to collections of items that have been grouped
     */
    def groupByKey[T, U, That <: Iterable[U]](builder: => Builder[U, That])(implicit ev: A <:< (T, U)): Map[T, That] =
      self.groupBy(_._1).mapVals(v => (builder ++= v.map(_._2)).result)
  }

  //////////////////////////////////////////////////////
  // ungroup(): Iterator[(A, B)]
  //   - For a map with collections for values, return an iterator of pairs
  //     where each key is paired with each item in its value collection
  //   - Equivalent to self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
  //////////////////////////////////////////////////////

  implicit class Enriched_ungroup_GenTraversableOnce[A, B](val self: GenTraversableOnce[(A, GenTraversableOnce[B])]) extends AnyVal {
    /**
     * For a map with collections for values, return an iterator of pairs
     * where each key is paired with each item in its value collection.
     *
     * Equivalent to self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
     *
     * @return an iterator of pairs
     */
    def ungroup() = self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
  }

  //////////////////////////////////////////////////////
  // dropRightWhile(p: A => Boolean): Repr
  //////////////////////////////////////////////////////

  implicit class Enriched_dropRightWhile_Seq[A, Repr](val self: SeqLike[A, Repr]) extends AnyVal {
    def dropRightWhile[That](p: A => Boolean)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      val buffer = mutable.Buffer[A]()
      for (x <- self) {
        buffer += x
        if (!p(x)) {
          b ++= buffer
          buffer.clear()
        }
      }
      b.result
    }
  }

  implicit class Enriched_dropRightWhile_String(val self: String) extends AnyVal {
    def dropRightWhile(p: Char => Boolean): String = {
      val b = StringCanBuildFrom()
      val buffer = mutable.Buffer[Char]()
      for (x <- self) {
        buffer += x
        if (!p(x)) {
          b ++= buffer
          buffer.clear()
        }
      }
      b.result
    }
  }

  //////////////////////////////////////////////////////
  // splitAt(n: Int) 
  //   - Split this collection at the specified index
  //   - Useful since Iterator.take doesn't guarantee the state of the original Iterator
  //   - Extend Traversable.splitAt to Iterator
  //////////////////////////////////////////////////////

  implicit class Enriched_splitAt_Iterator[A](val self: Iterator[A]) { // extends AnyVal {
    /**
     * Safely split this iterator at the specified index.  The 'first'
     * iterator must be exhausted completely before the items in the 'second'
     * iterator can be accessed.
     *
     * Inspired by Traversable.splitAt
     *
     * @param n	The index at which to split the collection
     * @return	a pair: the items before the split point and the items
     *          starting with the split point
     */
    def splitAt(n: Int): (Iterator[A], Iterator[A]) = {
      var i = 0

      val first: Iterator[A] =
        new Iterator[A] {
          def next(): A = {
            assert(hasNext, "first has already been read completely")
            i += 1; self.next
          }
          def hasNext() = i < n && self.hasNext
        }

      val second: Iterator[A] =
        new Iterator[A] {
          def next(): A = {
            assert(i >= n, "first has NOT YET been read completely")
            assert(hasNext, "second has already been read completely")
            i += 1; self.next
          }
          def hasNext() = self.hasNext
        }

      (first, second)
    }
  }

  /**
   * The KeepDelimiter enumeration is used to specify behavior for the `split` methods.
   */
  sealed trait KeepDelimiter
  object KeepDelimiter {
    case object DropDelimiter extends KeepDelimiter
    case object KeepDelimiterAsFirst extends KeepDelimiter
    case object KeepDelimiterAsLast extends KeepDelimiter
  }

  //////////////////////////////////////////////////////
  // split(delim: A): Iterator[Repr[A]]
  //   - Split this collection on each occurrence of the delimiter 
  //   - Inspired by String.split
  //////////////////////////////////////////////////////

  implicit class Enriched_split_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Split this collection on each occurrence of the delimiter.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split(delim: A, keepDelimiter: KeepDelimiter = KeepDelimiter.DropDelimiter): Iterator[Vector[A]] =
      split(delim, Vector.newBuilder[A], keepDelimiter)

    /**
     * Split this collection on each occurrence of the delimiter.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split[That](delim: A, builder: => Builder[A, That]): Iterator[That] =
      self.splitWhere(_ == delim, builder, KeepDelimiter.DropDelimiter)

    /**
     * Split this collection on each occurrence of the delimiter.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split[That](delim: A, builder: => Builder[A, That], keepDelimiter: KeepDelimiter): Iterator[That] =
      self.splitWhere(_ == delim, builder, keepDelimiter)
  }

  implicit class Enriched_split_Traversable[A, Repr](val self: TraversableLike[A, Repr]) extends AnyVal {
    /**
     * Split this collection on each occurrence of the delimiter.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split[That](delim: A, keepDelimiter: KeepDelimiter = KeepDelimiter.DropDelimiter)(implicit bf: CanBuildFrom[Repr, A, That]): Iterator[That] =
      self.toIterator.split(delim, bf(self.asInstanceOf[Repr]), keepDelimiter)
  }

  //////////////////////////////////////////////////////
  // splitWhere(p: A => Boolean): Iterator[Repr[A]]
  //   - Split this on items for which the predicate is true 
  //////////////////////////////////////////////////////

  implicit class Enriched_splitWhere_Iterator[A](val self: Iterator[A]) { // extends AnyVal {
    /**
     * Split this on items for which the predicate is true.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere(p: A => Boolean, keepDelimiter: KeepDelimiter = KeepDelimiter.DropDelimiter): Iterator[Vector[A]] =
      splitWhere(p, Vector.newBuilder[A], keepDelimiter)

    /**
     * Split this on items for which the predicate is true.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean, builder: => Builder[A, That]): Iterator[That] =
      splitWhere(p, builder, KeepDelimiter.DropDelimiter)

    /**
     * Split this on items for which the predicate is true.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean, builder: => Builder[A, That], keepDelimiter: KeepDelimiter): Iterator[That] =
      new Iterator[That] {
        var queued: Option[That] = None
        val bldr = new BuilderHolder(builder)

        def next(): That = {
          assert(this.hasNext, "next on empty iterator")
          val group = queued.get
          queued = None
          group
        }

        def hasNext() = {
          if (queued.isEmpty) {
            takeUntilDelim()
          }
          if (queued.isEmpty && bldr.nonEmpty) {
            queued = Some(bldr.result)
            bldr.clear()
          }
          queued.nonEmpty
        }

        @tailrec
        private def takeUntilDelim() {
          if (self.hasNext) {
            val x = self.next
            if (p(x)) {
              if (keepDelimiter == KeepDelimiter.KeepDelimiterAsLast) {
                bldr += x
              }
              queued = Some(bldr.result)
              bldr.clear()
              if (keepDelimiter == KeepDelimiter.KeepDelimiterAsFirst) {
                bldr += x
              }
            }
            else {
              bldr += x
              takeUntilDelim()
            }
          }
        }
      }
  }

  class BuilderHolder[A, That](builder: => Builder[A, That]) {
    private val b = builder
    private var bEmpty = true

    def +=(a: A) = {
      b += a
      bEmpty = false
      this
    }

    def result() = {
      b.result
    }

    def clear() {
      b.clear()
      bEmpty = true
    }

    def isEmpty = bEmpty
    def nonEmpty = !isEmpty

    override def toString() = s"BuilderHolder(${b.result}, isEmpty=$isEmpty)"
  }

  implicit class Enriched_splitWhere_Traversable[A, Repr](val self: TraversableLike[A, Repr]) extends AnyVal {
    /**
     * Split this on items for which the predicate is true.  Delimiters
     * do not appear in the output.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean, keepDelimiter: KeepDelimiter = KeepDelimiter.DropDelimiter)(implicit bf: CanBuildFrom[Repr, A, That]): Iterator[That] =
      self.toIterator.splitWhere(p, bf(self.asInstanceOf[Repr]), keepDelimiter)
  }

  //////////////////////////////////////////////////////
  // zipSafe(that: GenTraversable[B]): Repr[(A,B)]
  //   - zip this collection with another, throwing an exception if they are
  //     not of equal length.
  //////////////////////////////////////////////////////

  implicit class Enriched_zipSafe_Iterator[A](val self: Iterator[A]) { // extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe[B](that: GenTraversableOnce[B]) = {
      val thatItr = that.toIterator
      new Iterator[(A, B)] {
        def hasNext() = {
          val hn = self.hasNext
          assert(hn == thatItr.hasNext, s"Attempting to zipSafe collections of different lengths.  ${if (hn) "Second" else "First"} ran out.")
          hn
        }
        def next() = {
          hasNext()
          (self.next, thatItr.next)
        }
      }
    }
  }

  implicit class Enriched_zipSafe_GenTraversable[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe[A1 >: A, B, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      b ++= (self.toIterator zipSafe that)
      b.result
    }
  }

  implicit class Enriched_zipSafe_Tuple_of_Iterator[A, B](val self: (Iterator[A], GenTraversableOnce[B])) extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe = self._1 zipSafe self._2
  }

  implicit class Enriched_zipSafe_Tuple_of_GenTraversable[A, Repr, B](val self: (GenTraversableLike[A, Repr], GenTraversableOnce[B])) extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe[A1 >: A, That](implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(self._1.asInstanceOf[Repr])
      b ++= (self._1.toIterator zipSafe self._2)
      b.result
    }
  }

  //////////////////////////////////////////////////////
  // unzip 
  //   - Unzip this iterator of pairs into two iterators.
  //   - The new iterators coordinate to maintain laziness.
  //////////////////////////////////////////////////////

  implicit class Enriched_unzip2_Iterator[A, B](val self: Iterator[(A, B)]) { // extends AnyVal {
    abstract class QueuedPairIterator[T, O](thisQueue: mutable.Queue[T], otherQueue: mutable.Queue[O]) extends Iterator[T] {
      protected[this] def swapOrNot(p: (A, B)): (T, O)
      override def hasNext = thisQueue.nonEmpty || self.hasNext
      override def next =
        if (thisQueue.nonEmpty) thisQueue.dequeue()
        else { val (t, o) = swapOrNot(self.next()); otherQueue.enqueue(o); t }
    }
    def unzip(): (Iterator[A], Iterator[B]) = {
      val aQueue = mutable.Queue[A]()
      val bQueue = mutable.Queue[B]()
      val aItr = new QueuedPairIterator(aQueue, bQueue) { override def swapOrNot(p: (A, B)) = p }
      val bItr = new QueuedPairIterator(bQueue, aQueue) { override def swapOrNot(p: (A, B)) = p.swap }
      (aItr, bItr)
    }
  }

  //////////////////////////////////////////////////////
  // mapTo[B](f: A => B): Repr[(A,B)]
  //   - Map a function over the collection, returning a set of pairs consisting 
  //     of the original item and the result of the function application
  //   - Functionally equivalent to:
  //         map(x => x -> f(x))
  //////////////////////////////////////////////////////

  implicit class Enriched_mapTo_GenTraversableLike[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Map a function over the collection, returning a set of pairs consisting
     * of the original item and the result of the function application
     *
     * Functionally equivalent to: map(x => x -> f(x))
     *
     * @param f	the function to map
     * @return the new collection
     */
    def mapTo[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, (A, B), That]): That = {
      self.map(x => x -> f(x))
    }
  }

  implicit class Enriched_mapTo_Iterator[A](val self: Iterator[A]) { // extends AnyVal {
    /**
     * Map a function over the collection, returning a set of pairs consisting
     * of the original item and the result of the function application
     *
     * Functionally equivalent to: map(x => x -> f(x))
     *
     * @param f	the function to map
     * @return a new iterator
     */
    def mapTo[B](f: A => B): Iterator[(A, B)] = new Iterator[(A, B)] {
      def hasNext = self.hasNext
      def next() = {
        val x = self.next
        x -> f(x)
      }
    }
  }

  //////////////////////////////////////////////////////
  // mapToVal[B](v: B): Repr[(A,B)]
  //   - Map each item in the collection to a particular value
  //   - Functionally equivalent to:
  //         map(x => x -> v)
  //////////////////////////////////////////////////////

  implicit class Enriched_mapToVal_GenTraversableLike[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Map each item in the collection to a particular value
     *
     * Functionally equivalent to: map(x => x -> v)
     *
     * @param v	the value to map to
     * @return the new collection
     */
    def mapToVal[B, That](v: => B)(implicit bf: CanBuildFrom[Repr, (A, B), That]): That = {
      self.map(_ -> v)
    }
  }

  implicit class Enriched_mapToVal_Iterator[A](val self: Iterator[A]) { // extends AnyVal {
    /**
     * Map each item in the collection to a particular value
     *
     * Functionally equivalent to: map(x => x -> v)
     *
     * @param v	the value to map to
     * @return a new iterator
     */
    def mapToVal[B](v: => B): Iterator[(A, B)] = new Iterator[(A, B)] {
      def hasNext = self.hasNext
      def next() = self.next -> v
    }
  }

  //////////////////////////////////////////////////////
  // mapKeys(f: T => R): Repr[(R,U)]
  //   - In a collection of pairs, map a function over the first item of each pair.
  //   - Functionally equivalent to:
  //         this.map{case (k,v) => f(k) -> v}
  //////////////////////////////////////////////////////

  implicit class Enriched_mapKeys_GenTraversable[T, U, Repr](val self: GenTraversableLike[(T, U), Repr]) extends AnyVal {
    /**
     * In a collection of pairs, map a function over the first item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f	function to map over the first item of each pair
     * @return a collection of pairs
     */
    def mapKeys[R, That](f: T => R)(implicit bf: CanBuildFrom[Repr, (R, U), That]) = {
      self.map(x => f(x._1) -> x._2)
    }
  }

  implicit class Enriched_mapKeys_Iterator[T, U](val self: Iterator[(T, U)]) { // extends AnyVal {
    /**
     * In a collection of pairs, map a function over the first item of each
     * pair.
     *
     * @param f	function to map over the first item of each pair
     * @return a collection of pairs
     */
    def mapKeys[R](f: T => R) = new Iterator[(R, U)] {
      def hasNext = self.hasNext
      def next() = {
        val (k, v) = self.next()
        f(k) -> v
      }
    }
  }

  //////////////////////////////////////////////////////
  // mapVals(f: U => R): Repr[(T,R)]
  //   - In a collection of pairs, map a function over the second item of each pair.
  //   - Ensures that the map is computed at call-time, and not returned as a view as `Map.mapValues` would do.
  //   - Equivalent to: this.map { case (k,v) => k -> f(v) }
  //////////////////////////////////////////////////////

  implicit class Enriched_mapVals_GenTraversable[T, U, Repr](val self: GenTraversableLike[(T, U), Repr]) extends AnyVal {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f	function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapVals[R, That](f: U => R)(implicit bf: CanBuildFrom[Repr, (T, R), That]) = {
      self.map(x => x._1 -> f(x._2))
    }
  }

  implicit class Enriched_mapVals_Iterator[T, U](val self: Iterator[(T, U)]) { // extends AnyVal {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.
     *
     * @param f	function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapVals[R](f: U => R) = new Iterator[(T, R)] {
      def hasNext = self.hasNext
      def next() = {
        val (k, v) = self.next()
        k -> f(v)
      }
    }
  }

  //////////////////////////////////////////////////////
  // submap(f: T => R): Repr[R]
  //   - In a collection of collections, map a function over the inner collections without flattening.
  //   - Equivalent to: this.map(_.map(x => f(x)))
  //////////////////////////////////////////////////////

  implicit class Enriched_submap_GenTraversable[T, TRepr, SRepr](val self: GenTraversableLike[GenTraversableLike[T, TRepr], SRepr]) extends AnyVal {
    /**
     * In a collection of collections, map a function over the inner collections without flattening.
     *
     * @param f	function to map over the inner collections
     * @return a collection of collections
     */
    def submap[R, TThat, SThat](f: T => R)(implicit tbf: CanBuildFrom[TRepr, R, TThat], bf: CanBuildFrom[SRepr, TThat, SThat]) = {
      self.map(s => s.map(f)(tbf))
    }
  }

  implicit class Enriched_submap_Iterator[T, Repr](val self: Iterator[GenTraversableLike[T, Repr]]) { // extends AnyVal {
    /**
     * In a collection of collections, map a function over the inner collections without flattening.
     *
     * @param f	function to map over the inner collections
     * @return a collection of collections
     */
    def submap[R, That](f: T => R)(implicit bf: CanBuildFrom[Repr, R, That]) = new Iterator[That] {
      def hasNext = self.hasNext
      def next() = self.next().map(f)
    }
  }

  //////////////////////////////////////////////////////
  // mapt[A,B,R](f: (A,B) => R): Repr[R]
  //   - map over a Tuple2
  //   - same as `xs.map { case (x,y) => f(x,y) } `
  //////////////////////////////////////////////////////

  implicit class Enriched_mapt_2_GenTraversableLike[A, B, Repr](val self: GenTraversableLike[(A, B), Repr]) extends AnyVal {
    def mapt[R, That](f: (A, B) => R)(implicit bf: CanBuildFrom[Repr, R, That]) = {
      self.map(x => f(x._1, x._2))
    }
  }

  implicit class Enriched_mapt_2_Iterator[A, B](val self: Iterator[(A, B)]) { // extends AnyVal {
    def mapt[R](f: (A, B) => R) = new Iterator[R] {
      def next() = {
        val x = self.next
        f(x._1, x._2)
      }
      def hasNext() = self.hasNext
    }
  }

  implicit class Enriched_mapt_3_GenTraversableLike[A, B, C, Repr](val self: GenTraversableLike[(A, B, C), Repr]) extends AnyVal {
    def mapt[R, That](f: (A, B, C) => R)(implicit bf: CanBuildFrom[Repr, R, That]) = {
      self.map(x => f(x._1, x._2, x._3))
    }
  }

  implicit class Enriched_mapt_3_Iterator[A, B, C](val self: Iterator[(A, B, C)]) { // extends AnyVal {
    def mapt[R](f: (A, B, C) => R) = new Iterator[R] {
      def next() = {
        val x = self.next
        f(x._1, x._2, x._3)
      }
      def hasNext() = self.hasNext
    }
  }

  //////////////////////////////////////////////////////
  // foldLeftWhile[A,B](z: B)(p: (B, A) => Boolean)(op: (B, A) => B): B
  //   - Folds while the condition `p` is true.
  //   - If `p` operates on the item, then it behaves like `takeWhile`;
  //     if it operates on the accumulator, then it behaves like `while`.
  //////////////////////////////////////////////////////

  implicit class Enriched_foldLeftWhile_GenTraversableOnce[A](val self: GenTraversableOnce[A]) extends AnyVal {
    def foldLeftWhile[B](z: B)(p: (B, A) => Boolean)(op: (B, A) => B): B = {
      var result = z
      val it = self.toIterator
      while (it.hasNext) {
        val x = it.next
        if (!p(result, x)) return result
        result = op(result, x)
      }
      result
    }
  }

  //////////////////////////////////////////////////////
  // avg(): A
  //   - Find the average (mean) of this collection of numbers
  //////////////////////////////////////////////////////

  implicit class Enrich_avg_GenTraversableOnce[A](val self: GenTraversableOnce[A])(implicit num: Fractional[A]) {
    /**
     * Find the average (mean) of this collection of numbers.
     *
     * @return the average (mean)
     */
    def avg = {
      assert(self.nonEmpty, "cannot average an empty collection")
      val (total, count) = self.foldLeft((num.zero, num.zero)) {
        case ((total, count), x) => (num.plus(total, x), num.plus(count, num.one))
      }
      num.div(total, count)
    }
  }

  implicit class Enrich_avg_Int_GenTraversableOnce(val self: GenTraversableOnce[Int]) extends AnyVal {
    /**
     * Find the average (mean) of this collection of numbers.
     *
     * @return the average (mean)
     */
    def avg = {
      assert(self.nonEmpty, "cannot average an empty collection")
      val (total, count) = self.foldLeft((0, 0)) {
        case ((total, count), x) => (total + x, count + 1)
      }
      total.toDouble / count
    }
  }

  //////////////////////////////////////////////////////
  // normalize(): Repr[A]
  //   - Normalize this collection of numbers by dividing each by the sum
  //////////////////////////////////////////////////////

  implicit class Enriched_normalize_GenTraversable[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Normalize this collection of numbers by dividing each by the sum
     *
     * @return normalized values
     */
    def normalize[That](implicit num: Fractional[A], bf: CanBuildFrom[Repr, A, That]) = {
      assert(self.nonEmpty, "cannot normalize an empty collection")
      val total = self.sum
      self.map(num.div(_, total))
    }
  }

  implicit class Enriched_normalize_Int_GenTraversable[Repr](val self: GenTraversableLike[Int, Repr]) extends AnyVal {
    /**
     * Normalize this collection of numbers by dividing each by the sum
     *
     * @return normalized values
     */
    def normalize[That](implicit bf: CanBuildFrom[Repr, Double, That]) = {
      assert(self.nonEmpty, "cannot average an empty collection")
      val total = self.sum.toDouble
      self.map(_ / total)
    }
  }

  //////////////////////////////////////////////////////
  // normalizeValues(): Repr[(T,U)]
  //   - Normalize this values in this collection of pairs
  //////////////////////////////////////////////////////

  implicit class Enriched_normalizeValues_GenTraversable[T, U, Repr](val self: GenTraversableLike[(T, U), Repr]) extends AnyVal {
    /**
     * Normalize this values in this collection of pairs
     *
     * @return a collection of pairs
     */
    def normalizeValues[That](implicit num: Fractional[U], bf: CanBuildFrom[Repr, (T, U), That]) = {
      val total = self.foldLeft(num.zero)((z, a) => num.plus(z, a._2))
      self.map(x => x._1 -> num.div(x._2, total))
    }
  }

  implicit class Enriched_normalizeValues_Int_GenTraversable[T, Repr](val self: GenTraversableLike[(T, Int), Repr]) extends AnyVal {
    /**
     * Normalize this values in this collection of pairs
     *
     * @return a collection of pairs
     */
    def normalizeValues[That](implicit bf: CanBuildFrom[Repr, (T, Double), That]) = {
      val total = self.foldLeft(0)((z, a) => z + a._2).toDouble
      self.map(x => x._1 -> (x._2 / total))
    }
  }

  //////////////////////////////////////////////////////
  // maxByN(f: A => B, n: Int): Repr[A]
  //   - Equivalent to, but faster than self.sortBy(f).takeRight(n).reverse
  //////////////////////////////////////////////////////

  implicit class Enriched_maxByN_GenTraversable[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Find the N maximum entries in the collection.
     *
     * @return a collection containing the N maximum entries, sorted so the max is first
     */
    def maxByN[B, That](f: A => B, n: Int)(implicit ord: Ordering[B], bf: CanBuildFrom[Repr, A, That]): That = {
      val r = new Array[(A, B)](n min self.size)
      val it = self.toIterator
      var i = 0
      while (it.hasNext) {
        val x = it.next
        val b = f(x)
        var j = 0
        while (j < (i min n) && ord.gteq(r(j)._2, b)) { // while the thing at position j is greater than f(x)
          j += 1 // keep looking for the insertion position
        }
        if (j < n) { // if the insertion position is in the array
          for (j2 <- (i min (n - 1) until j by -1)) { // move each lower value down one array position
            r(j2) = r(j2 - 1)
          }
          r(j) = (x -> b) // insert x in the correct place
        }
        i += 1
      }
      (bf(self.asInstanceOf[Repr]) ++= r.map(_._1)).result
    }
  }

  implicit class Enriched_maxByN_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Find the N maximum entries in the collection.
     *
     * @return a collection containing the N maximum entries, sorted so the max is first
     */
    def maxByN[B](f: A => B, n: Int)(implicit ord: Ordering[B]): Vector[A] = {
      val r = new Array[(A, B)](n)
      val it = self.toIterator
      var i = 0
      while (it.hasNext) {
        val x = it.next
        val b = f(x)
        var j = 0
        while (j < (i min n) && ord.gteq(r(j)._2, b)) { // while the thing at position j is greater than f(x)
          j += 1 // keep looking for the insertion position
        }
        if (j < n) { // if the insertion position is in the array
          for (j2 <- (i min (n - 1) until j by -1)) { // move each lower value down one array position
            r(j2) = r(j2 - 1)
          }
          r(j) = (x -> b) // insert x in the correct place
        }
        i += 1
      }
      (Vector.newBuilder ++= r.take(i min n).map(_._1)).result
    }
  }

  //////////////////////////////////////////////////////
  // minByN(f: A => B, n: Int): Repr[A]
  //   - Equivalent to, but faster than self.sortBy(f).take(n)
  //////////////////////////////////////////////////////

  implicit class Enriched_minByN_GenTraversable[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Find the N minimum entries in the collection.
     *
     * @return a collection containing the N minimum entries, sorted so the min is first
     */
    def minByN[B, That](f: A => B, n: Int)(implicit ord: Ordering[B], bf: CanBuildFrom[Repr, A, That]): That = {
      val r = new Array[(A, B)](n min self.size)
      val it = self.toIterator
      var i = 0
      while (it.hasNext) {
        val x = it.next
        val b = f(x)
        var j = 0
        while (j < (i min n) && ord.lteq(r(j)._2, b)) { // while the thing at position j is greater than f(x)
          j += 1 // keep looking for the insertion position
        }
        if (j < n) { // if the insertion position is in the array
          for (j2 <- (i min (n - 1) until j by -1)) { // move each lower value down one array position
            r(j2) = r(j2 - 1)
          }
          r(j) = (x -> b) // insert x in the correct place
        }
        i += 1
      }
      (bf(self.asInstanceOf[Repr]) ++= r.map(_._1)).result
    }
  }

  implicit class Enriched_minByN_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Find the N minimum entries in the collection.
     *
     * @return a collection containing the N minimum entries, sorted so the min is first
     */
    def minByN[B](f: A => B, n: Int)(implicit ord: Ordering[B]): Vector[A] = {
      val r = new Array[(A, B)](n)
      val it = self.toIterator
      var i = 0
      while (it.hasNext) {
        val x = it.next
        val b = f(x)
        var j = 0
        while (j < (i min n) && ord.lteq(r(j)._2, b)) { // while the thing at position j is greater than f(x)
          j += 1 // keep looking for the insertion position
        }
        if (j < n) { // if the insertion position is in the array
          for (j2 <- (i min (n - 1) until j by -1)) { // move each lower value down one array position
            r(j2) = r(j2 - 1)
          }
          r(j) = (x -> b) // insert x in the correct place
        }
        i += 1
      }
      (Vector.newBuilder ++= r.take(i min n).map(_._1)).result
    }
  }

  //////////////////////////////////////////////////////
  // shuffle
  //////////////////////////////////////////////////////

  implicit class Enriched_shuffle_Seq[A, Repr](val self: SeqLike[A, Repr]) extends AnyVal {
    def shuffle[That](implicit bf: CanBuildFrom[Repr, A, That]): That =
      (bf(self.asInstanceOf[Repr]) ++= Random.shuffle(self)).result
  }

  implicit class Enriched_shuffle_Iterator[A](val self: Iterator[A]) extends AnyVal {
    def shuffle: Iterator[A] = Random.shuffle(self)
  }

  //////////////////////////////////////////////////////
  // toBitSet
  //////////////////////////////////////////////////////

  implicit class Enriched_toBitSet_GenTraversableOnce(val self: GenTraversableOnce[Int]) extends AnyVal {
    def toBitSet: BitSet = BitSet() ++ self
  }

  //////////////////////////////////////////////////////
  // PARALLEL / SEQUENTIAL
  //   - obnoxious versions of the .par and .seq methods 
  //////////////////////////////////////////////////////

  implicit class Enriched_PARALLEL_Parallelizable[+A, +ParRepr <: Parallel](val self: Parallelizable[A, ParRepr]) extends AnyVal {
    def PARALLEL = self.par
  }
  implicit class Enriched_SEQUENTIAL_GenTraversableOnce[+A](val self: GenTraversableOnce[A]) extends AnyVal {
    def SEQUENTIAL = self.seq
  }

}
