package dhg.util

import scala.collection.GenIterable
import scala.collection.GenIterableLike
import scala.collection.GenTraversable
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.IterableLike
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.Builder
import scala.util.Random
import scala.annotation.tailrec
import scala.collection.generic.IsTraversableOnce
import scala.collection.generic.IsTraversableLike
import scala.collection.SeqLike

/**
 * @author Dan Garrette (dhg@cs.utexas.edu)
 */
object CollectionUtil {

  //////////////////////////////////////////////////////
  // toTuple2: (T,T)
  // toTuple3: (T,T,T)
  // toTuple4: (T,T,T,T)
  // toTuple5: (T,T,T,T,T)
  //   - Convert this sequence to a tuple
  //////////////////////////////////////////////////////

  implicit class Enriched_toTuple_Seq[A](seq: Seq[A]) {
    def toTuple2 = seq match { case Seq(a, b) => (a, b); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple2: $x") }
    def toTuple3 = seq match { case Seq(a, b, c) => (a, b, c); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple3: $x") }
    def toTuple4 = seq match { case Seq(a, b, c, d) => (a, b, c, d); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple4: $x") }
    def toTuple5 = seq match { case Seq(a, b, c, d, e) => (a, b, c, d, e); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple5: $x") }
  }

  implicit class Enriched_toTuple_Array[A](seq: Array[A]) {
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

  implicit class Enriched_prependAppend_Iterator[A](self: Iterator[A]) {
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

  implicit class Enriched_counts_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    /**
     * Map each distinct item in the collection to the number of times it appears.
     *
     * @return Map from items to their counts
     */
    def counts(): Map[A, Int] = {
      val m = mutable.Map.empty[A, Int]
      for (item <- self) {
        val count = m.getOrElseUpdate(item, 0)
        m(item) = count + 1
      }
      m.toMap
    }
  }

  //////////////////////////////////////////////////////
  // groupByKey(): Map[T,Repr[U]]
  //   - For a collection of pairs (k,v), create a map from each `k` to the  
  //     collection of `v`s with which it is associated.
  //   - Equivalent to self.groupBy(_._1).map { case (k, elems) => (k, elems.map(_._2)) }
  //////////////////////////////////////////////////////

  implicit class Enriched_groupByKey_Traversable[A, Repr <: Traversable[A]](self: TraversableLike[A, Repr]) {
    /**
     * For a collection of pairs (k,v), create a map from each `k` to the
     * collection of `v`s with which it is associated.
     *
     * Equivalent to self.groupBy(_._1).map { case (k, elems) => (k, elems.map(_._2)) }
     *
     * @return Map from `k`s to collections of `v`s
     */
    def groupByKey[K, V, That](implicit ev: A <:< (K, V), bf: CanBuildFrom[Repr, V, That]): Map[K, That] = {
      val m = mutable.Map.empty[K, Builder[V, That]]
      for ((key, value) <- self.map(ev)) {
        val bldr = m.getOrElseUpdate(key, bf(self.asInstanceOf[Repr]))
        bldr += value
      }
      val b = immutable.Map.newBuilder[K, That]
      for ((k, v) <- m)
        b += ((k, v.result))
      b.result
    }
  }

  //////////////////////////////////////////////////////
  // ungroup(): Iterator[(A, B)]
  //   - For a map with collections for values, return an iterator of pairs
  //     where each key is paired with each item in its value collection
  //   - Equivalent to self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
  //////////////////////////////////////////////////////

  implicit class Enriched_ungroup_GenTraversableOnce[A, B](self: GenTraversableOnce[(A, GenTraversableOnce[B])]) {
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

  implicit class Enriched_dropRightWhile_Seq[A, Repr](self: SeqLike[A, Repr]) {
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

  implicit class Enriched_dropRightWhile_String(self: String) {
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

  implicit class Enriched_splitAt_Iterator[A](self: Iterator[A]) {
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

  //////////////////////////////////////////////////////
  // split(delim: A): Iterator[Repr[A]]
  //   - Split this collection on each occurrence of the delimiter 
  //   - Inspired by String.split
  //////////////////////////////////////////////////////

  implicit class Enriched_split_Iterator[A](self: Iterator[A]) {
    /**
     * Split this collection on each occurrence of the delimiter.  Delimiters
     * do not appear in the output.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split(delim: A): Iterator[Vector[A]] =
      split(delim, Vector.newBuilder[A])

    /**
     * Split this collection on each occurrence of the delimiter.  Delimiters
     * do not appear in the output.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split[That](delim: A, builder: => Builder[A, That]): Iterator[That] =
      self.splitWhere(_ == delim, builder)
  }

  implicit class Enriched_split_GenTraversable[A, Repr](self: GenTraversableLike[A, Repr]) {
    /**
     * Split this collection on each occurrence of the delimiter.  Delimiters
     * do not appear in the output.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split[That](delim: A)(implicit bf: CanBuildFrom[Repr, A, That]): Iterator[That] =
      self.toIterator.split(delim, bf(self.asInstanceOf[Repr]))
  }

  //////////////////////////////////////////////////////
  // splitWhere(p: A => Boolean): Iterator[Repr[A]]
  //   - Split this on items for which the predicate is true 
  //////////////////////////////////////////////////////

  implicit class Enriched_splitWhere_Iterator[A](self: Iterator[A]) {
    /**
     * Split this on items for which the predicate is true.  Delimiters
     * do not appear in the output.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere(p: A => Boolean): Iterator[Vector[A]] =
      splitWhere(p, Vector.newBuilder[A])

    /**
     * Split this on items for which the predicate is true.  Delimiters
     * do not appear in the output.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean, builder: => Builder[A, That]): Iterator[That] =
      new Iterator[That] {
        var buffer = mutable.Queue[That]()
        def next(): That = {
          assert(this.hasNext, "next on empty iterator")
          buffer.dequeue
        }

        def hasNext() = {
          if (buffer.isEmpty && self.hasNext) {
            takeUntilNext(builder)
          }
          buffer.nonEmpty
        }

        @tailrec
        private def takeUntilNext(builder: => Builder[A, That]): Unit = {
          val (empty, item) = takeUntilDelim(builder)
          buffer.enqueue(item)
          if (empty)
            if (self.hasNext)
              takeUntilNext(builder)
            else
              buffer.clear()
        }

        @tailrec
        private def takeUntilDelim(b: Builder[A, That], empty: Boolean = true): (Boolean, That) = {
          if (self.hasNext) {
            val x = self.next
            if (p(x))
              (empty, b.result)
            else
              takeUntilDelim(b += x, false)
          }
          else
            (empty, b.result)
        }
      }
  }

  implicit class Enriched_splitWhere_GenTraversable[A, Repr](self: GenTraversableLike[A, Repr]) {
    /**
     * Split this on items for which the predicate is true.  Delimiters
     * do not appear in the output.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean)(implicit bf: CanBuildFrom[Repr, A, That]): Iterator[That] =
      self.toIterator.splitWhere(p, bf(self.asInstanceOf[Repr]))
  }

  //////////////////////////////////////////////////////
  // zipSafe(that: GenTraversable[B]): Repr[(A,B)]
  //   - zip this collection with another, throwing an exception if they are
  //     not of equal length.
  //////////////////////////////////////////////////////

  implicit class Enriched_zipSafe_Iterator[A](self: Iterator[A]) {
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
        def hasNext = {
          val hn = self.hasNext
          assert(hn == thatItr.hasNext, "Attempting to zipSafe collections of different lengths.")
          hn
        }
        def next() = {
          assert(self.hasNext == thatItr.hasNext, "Attempting to zipSafe collections of different lengths.")
          (self.next, thatItr.next)
        }
      }
    }
  }

  implicit class Enriched_zipSafe_GenTraversable[A, Repr](self: GenTraversableLike[A, Repr]) {
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

  implicit class Enriched_zipSafe_Tuple_of_Iterator[A, B](self: (Iterator[A], GenTraversableOnce[B])) {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe = self._1 zipSafe self._2
  }

  implicit class Enriched_zipSafe_Tuple_of_GenTraversable[A, Repr, B](self: (GenTraversableLike[A, Repr], GenTraversableOnce[B])) {
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
  // unzip(): (Iterator[A], Iterator[B])
  //   - Extend unzip functionality to Iterator
  //////////////////////////////////////////////////////

  implicit class Enriched_unzip_Iterator[T, U](self: Iterator[(T, U)]) {
    def unzip(): (Vector[T], Vector[U]) =
      this.unzip(Vector.newBuilder[T], Vector.newBuilder[U])

    def unzip[ThatT <: Iterable[T], ThatU <: Iterable[U]](tBuilder: => Builder[T, ThatT], uBuilder: => Builder[U, ThatU]): (ThatT, ThatU) = {
      val tBldr = tBuilder
      val uBldr = uBuilder
      for ((t, u) <- self) {
        tBldr += t
        uBldr += u
      }
      (tBldr.result, uBldr.result)
    }
  }

  //////////////////////////////////////////////////////
  // mapTo[B](f: A => B): Repr[(A,B)]
  //   - Map a function over the collection, returning a set of pairs consisting 
  //     of the original item and the result of the function application
  //   - Functionally equivalent to:
  //         map(x => x -> f(x))
  //////////////////////////////////////////////////////

  implicit class Enriched_mapTo_GenTraversableLike[A, Repr](self: GenTraversableLike[A, Repr]) {
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
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for (x <- self) b += x -> f(x)
      b.result
    }
  }

  implicit class Enriched_mapTo_Iterator[A](self: Iterator[A]) {
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

  implicit class Enriched_mapToVal_GenTraversableLike[A, Repr](self: GenTraversableLike[A, Repr]) {
    /**
     * Map each item in the collection to a particular value
     *
     * Functionally equivalent to: map(x => x -> v)
     *
     * @param v	the value to map to
     * @return the new collection
     */
    def mapToVal[B, That](v: => B)(implicit bf: CanBuildFrom[Repr, (A, B), That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for (x <- self) b += x -> v
      b.result
    }
  }

  implicit class Enriched_mapToVal_Iterator[A](self: Iterator[A]) {
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

  implicit class Enriched_mapKeys_GenTraversable[T, U, Repr](self: GenTraversableLike[(T, U), Repr]) {
    /**
     * In a collection of pairs, map a function over the first item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f	function to map over the first item of each pair
     * @return a collection of pairs
     */
    def mapKeys[R, That](f: T => R)(implicit bf: CanBuildFrom[Repr, (R, U), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for ((k, v) <- self) b += f(k) -> v
      b.result
    }
  }

  implicit class Enriched_mapKeys_Iterator[T, U](self: Iterator[(T, U)]) {
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

  implicit class Enriched_mapVals_GenTraversable[T, U, Repr](self: GenTraversableLike[(T, U), Repr]) {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f	function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapVals[R, That](f: U => R)(implicit bf: CanBuildFrom[Repr, (T, R), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for ((k, v) <- self) b += k -> f(v)
      b.result
    }
  }

  implicit class Enriched_mapVals_Iterator[T, U](self: Iterator[(T, U)]) {
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
  // mapt[A,B,R](f: (A,B) => R): Repr[R]
  //   - map over a Tuple2
  //   - same as `xs.map { case (x,y) => f(x,y) } `
  //////////////////////////////////////////////////////

  implicit class Enriched_mapt_2_GenTraversableLike[A, B, Repr](self: GenTraversableLike[(A, B), Repr]) {
    def mapt[R, That](f: (A, B) => R)(implicit bf: CanBuildFrom[Repr, R, That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for (x <- self) b += f(x._1, x._2)
      b.result
    }
  }

  implicit class Enriched_mapt_2_Iterator[A, B](self: Iterator[(A, B)]) {
    def mapt[R](f: (A, B) => R) = new Iterator[R] {
      override def next() = {
        val x = self.next
        f(x._1, x._2)
      }
      override def hasNext() = self.hasNext
    }
  }

  implicit class Enriched_mapt_3_GenTraversableLike[A, B, C, Repr](self: GenTraversableLike[(A, B, C), Repr]) {
    def mapt[R, That](f: (A, B, C) => R)(implicit bf: CanBuildFrom[Repr, R, That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for (x <- self) b += f(x._1, x._2, x._3)
      b.result
    }
  }

  implicit class Enriched_mapt_3_Iterator[A, B, C](self: Iterator[(A, B, C)]) {
    def mapt[R](f: (A, B, C) => R) = new Iterator[R] {
      override def next() = {
        val x = self.next
        f(x._1, x._2, x._3)
      }
      override def hasNext() = self.hasNext
    }
  }

  //////////////////////////////////////////////////////
  // avg(): A
  //   - Find the average (mean) of this collection of numbers
  //////////////////////////////////////////////////////

  implicit class Enrich_avg_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    /**
     * Find the average (mean) of this collection of numbers.
     *
     * @return the average (mean)
     */
    def avg(implicit num: Fractional[A]) = {
      val (total, count) = self.toIterator.foldLeft((num.zero, num.zero)) {
        case ((total, count), x) => (num.plus(total, x), num.plus(count, num.one))
      }
      num.div(total, count)
    }
  }

  implicit class Enrich_avg_Int_GenTraversableOnce(self: GenTraversableOnce[Int]) {
    /**
     * Find the average (mean) of this collection of numbers.
     *
     * @return the average (mean)
     */
    def avg = {
      val (total, count) = self.toIterator.foldLeft((0, 0)) {
        case ((total, count), x) => (total + x, count + 1)
      }
      total.toDouble / count
    }
  }

  //////////////////////////////////////////////////////
  // normalize(): Repr[A]
  //   - Normalize this collection of numbers by dividing each by the sum
  //////////////////////////////////////////////////////

  implicit class Enriched_normalize_GenTraversable[A, Repr](self: GenTraversableLike[A, Repr]) {
    /**
     * Normalize this collection of numbers by dividing each by the sum
     *
     * @return normalized values
     */
    def normalize[That](implicit num: Fractional[A], bf: CanBuildFrom[Repr, A, That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.sum
      for (x <- self) b += num.div(x, total)
      b.result
    }
  }

  implicit class Enriched_normalize_Int_GenTraversable[Repr](self: GenTraversableLike[Int, Repr]) {
    /**
     * Normalize this collection of numbers by dividing each by the sum
     *
     * @return normalized values
     */
    def normalize[That](implicit bf: CanBuildFrom[Repr, Double, That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.sum.toDouble
      for (x <- self) b += x / total
      b.result
    }
  }

  //////////////////////////////////////////////////////
  // normalizeValues(): Repr[(T,U)]
  //   - Normalize this values in this collection of pairs
  //////////////////////////////////////////////////////

  implicit class Enriched_normalizeValues_GenTraversable[T, U, Repr](self: GenTraversableLike[(T, U), Repr]) {
    /**
     * Normalize this values in this collection of pairs
     *
     * @return a collection of pairs
     */
    def normalizeValues[That](implicit num: Fractional[U], bf: CanBuildFrom[Repr, (T, U), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.foldLeft(num.zero)((z, a) => num.plus(z, a._2))
      for ((k, v) <- self) b += k -> num.div(v, total)
      b.result
    }
  }

  implicit class Enriched_normalizeValues_Int_GenTraversable[T, Repr](self: GenTraversableLike[(T, Int), Repr]) {
    /**
     * Normalize this values in this collection of pairs
     *
     * @return a collection of pairs
     */
    def normalizeValues[That](implicit bf: CanBuildFrom[Repr, (T, Double), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.foldLeft(0)((z, a) => z + a._2).toDouble
      for ((k, v) <- self) b += k -> (v / total)
      b.result
    }
  }

}
