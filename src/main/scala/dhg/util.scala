package dhg

import java.awt.BorderLayout

import org.abego.treelayout.netbeans.AbegoTreeLayoutForNetbeans
import org.netbeans.api.visual.anchor.AnchorFactory
import org.netbeans.api.visual.graph.GraphScene
import org.netbeans.api.visual.layout.LayoutFactory
import org.netbeans.api.visual.widget.ConnectionWidget
import org.netbeans.api.visual.widget.LabelWidget
import org.netbeans.api.visual.widget.LayerWidget
import org.netbeans.api.visual.widget.Widget
import javax.swing.JDialog
import javax.swing.JScrollPane
import java.io.Closeable
import scala.io.Source
import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversableLike
import scala.collection.Parallel
import scala.collection.GenTraversable
import scalaz.{ Ordering => _, _ }
import Scalaz._
import scala.annotation.tailrec
import scala.collection.GenSeqLike
import scala.collection.GenTraversable
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.Parallel
import scala.collection.Parallelizable
import scala.collection.SeqLike
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.Builder
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.io.BufferedWriter
import java.io.File
import java.io.File.createTempFile
import java.io.File.separator
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.OutputStreamWriter
import java.io.Writer
import java.net.URI
import scala.collection.breakOut
import scala.io.BufferedSource
import scala.io.Source
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import scala.util.matching.Regex
import scala.math._
import scala.sys.process._
import java.io.PrintStream
import scala.collection.generic.Growable
import java.io.Writer
import scala.sys.process._
import java.io.PrintStream
import scala.collection.generic.Growable
import java.io.Writer
import dhg.util.Subprocess._
import scala.annotation.tailrec
import scala.collection.GenSeqLike
import scala.collection.GenTraversable
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.Parallel
import scala.collection.Parallelizable
import scala.collection.SeqLike
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.Builder
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.annotation.tailrec
import scala.math.{ pow, exp, log }
import org.apache.commons.math3.random.{ RandomGenerator }
import dhg.util._
import java.awt.Color
import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.Plot
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.AbstractRenderer
import org.jfree.chart.renderer.category.StandardBarPainter
import org.jfree.chart.renderer.xy.StandardXYBarPainter
import org.jfree.chart.renderer.xy.XYBarRenderer
import org.jfree.data.category.DefaultCategoryDataset
import javax.swing.JFrame
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.data.xy.XYSeries
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.CategoryPlot
import org.jfree.data.general.Dataset
import org.jfree.chart.LegendItemSource
import org.jfree.chart.renderer.xy.XYItemRenderer
import org.jfree.chart.plot.DatasetRenderingOrder
import org.jfree.data.xy.{ XYDataset => JXYDataset }
import org.jfree.chart.axis.ValueAxis
import org.jfree.chart.axis.DateAxis
import scala.collection.GenTraversable
import java.awt.{ Shape => JShape }
import scala.collection.GenTraversableOnce
import dhg.util._
import java.lang.Comparable
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.data.xy.XYSeries
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.statistics.{ HistogramDataset => JfcHistogramDataset }
import org.jfree.data.xy.XYIntervalSeries
import org.jfree.data.xy.IntervalXYDataset
import org.jfree.data.xy.XYIntervalSeriesCollection
import scala.collection.GenTraversable
import scala.collection.mutable
import scala.collection.GenTraversableOnce
import java.awt.Color
import org.jfree.chart.renderer.xy.StandardXYBarPainter
import org.jfree.chart.renderer.xy.XYBarRenderer
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.BasicStroke
import java.awt.{ Shape => JShape }
import org.jfree.util.ShapeUtilities
import java.awt.geom.Ellipse2D
import org.junit.Assert._
import scala.math.log
import java.lang.AssertionError
import scala.util.matching.Regex
import org.apache.commons.math3.random.RandomGenerator

/**
 * The entire `util` package, basically.
 * It's like this so that you can just do `import dhg.util._`
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object util {

  //  implicit class NumericWithToLogDouble[N](self: N)(implicit num: Numeric[N]) {
  //    def toLogDouble = LogDouble(self)
  //    def log = toLogDouble
  //  }

  //////////////////////////////////
  // LogDouble.scala
  //////////////////////////////////

  /**
   * This Numeric class represents values using logarithms.  The underlying
   * logarithmic representation is completely hidden from the calling code.
   *
   * This class exists to allow for the use of obvious operators (* for
   * multiplication instead of + on logarithms) and to prevent coding mistakes
   * resulting from the inadvertent mixing of logarithmic and non-logarithmic
   * Double representations of probabilities.  Additionally, it is possible to
   * use the `sum` and `product` collection methods on collections of
   * LogDoubles, and get the expected results.
   *
   * All to* methods return the (non-logarithmic) value stored.  The only
   * way to access the actual logarithmic value is by the 'logValue' field.
   *
   * @author Dan Garrette (dhgarrette@gmail.com)
   */
  class LogDouble(val logValue: Double) extends AnyVal with Ordered[LogDouble] {
    def +(other: LogDouble): LogDouble = {
      val oLogValue = other.logValue
      if (logValue == Double.NegativeInfinity)
        other
      else if (oLogValue == Double.NegativeInfinity)
        this
      else if (logValue > oLogValue)
        new LogDouble(logValue + log1p(exp(oLogValue - logValue)))
      else
        new LogDouble(oLogValue + log1p(exp(logValue - oLogValue)))
    }

    def -(other: LogDouble): LogDouble = {
      val oLogValue = other.logValue
      if (oLogValue == 0.0)
        this
      else if (logValue < oLogValue)
        sys.error("subtraction results in a negative LogDouble")
      else
        new LogDouble(logValue + log1p(-exp(oLogValue - logValue)))
    }

    def *(other: LogDouble): LogDouble = new LogDouble(logValue + other.logValue)
    def /(other: LogDouble): LogDouble = new LogDouble(logValue - other.logValue)

    def **(pow: Int): LogDouble = new LogDouble(pow * logValue)
    def **(pow: Double): LogDouble = new LogDouble(pow * logValue)

    override def compare(that: LogDouble) = logValue.compare(that.logValue)
    def max(that: LogDouble): LogDouble = if (this.logValue > that.logValue) this else that
    def min(that: LogDouble): LogDouble = if (this.logValue < that.logValue) this else that

    def approx(o: LogDouble, tolerance: Double): Boolean = (logValue - o.logValue).abs < tolerance
    def approx(o: LogDouble): Boolean = this.approx(o, 1e-10)

    def isZero: Boolean = logValue.isNegInfinity
    def nonZero: Boolean = !logValue.isNegInfinity
    def isNaN: Boolean = logValue.isNaN
    def notNaN: Boolean = !logValue.isNaN
    def isInfinite: Boolean = logValue.isPosInfinity
    def nonInfinite: Boolean = !logValue.isPosInfinity

    def toInt = toDouble.toInt
    def toLong = toDouble.toLong
    def toFloat = toDouble.toFloat
    def toDouble = exp(logValue)

    override def toString = s"LogDouble(${toDouble})"
  }

  object LogDouble {

    def apply[N](n: N)(implicit num: Numeric[N]): LogDouble = {
      n match {
        case logDouble: LogDouble => logDouble
        case _ =>
          val d = num.toDouble(n)
          require(d >= 0.0, "cannot take log of a negative number")
          new LogDouble(log(d))
      }
    }

    def sum(xs: Seq[LogDouble]): LogDouble = {
      new LogDouble(FastMathUtil.logSum(xs.map(_.logValue)))
    }

    val zero = new LogDouble(Double.NegativeInfinity)
    val one = new LogDouble(0.0)

  }

  trait LogDoubleOrdering extends scala.math.Ordering[LogDouble] {
    override def compare(a: LogDouble, b: LogDouble) = a compare b
  }

  implicit object LogDoubleIsFractional extends LogDoubleIsFractional with LogDoubleOrdering

  trait LogDoubleIsFractional extends Fractional[LogDouble] {
    def plus(x: LogDouble, y: LogDouble): LogDouble = x + y
    def minus(x: LogDouble, y: LogDouble): LogDouble = x - y
    def times(x: LogDouble, y: LogDouble): LogDouble = x * y
    def div(x: LogDouble, y: LogDouble): LogDouble = x / y
    def negate(x: LogDouble): LogDouble = sys.error("LogDouble values cannot be negated")
    def fromInt(x: Int): LogDouble = new LogDouble(log(x))
    def toInt(x: LogDouble): Int = x.toInt
    def toLong(x: LogDouble): Long = x.toLong
    def toFloat(x: LogDouble): Float = x.toFloat
    def toDouble(x: LogDouble): Double = x.toDouble
    override def zero = LogDouble.zero
    override def one = LogDouble.one
  }

  implicit object LogDoubleSemigroup extends Semigroup[LogDouble] {
    override def append(f1: LogDouble, f2: => LogDouble) = f1 + f2
  }

  //////////////////////////////////
  // Arm.scala: Automatic Resource Management (ARM) utility.
  //////////////////////////////////

  trait Managed[T] {
    def self: T
    def close(): Unit
  }

  implicit class ManagedCloseable[T <: Closeable](val self: T) extends Managed[T] {
    def close() { self.close() }
  }

  implicit class ManagedSource[T <: Source](val self: T) extends Managed[T] {
    def close() { self.close() }
  }

  /**
   * Automatic Resource Management.  Ensure that the resource is closed after
   * executing the block.
   *
   * Example:
   *   using(new BufferedReader(new FileReader("file"))) { r =>
   *     var count = 0
   *     while (r.readLine != null) count += 1
   *     println(count)
   *   }
   */
  def using[T, R](resource: Managed[T])(block: T => R): R = {
    try {
      block(resource.self)
    }
    finally {
      resource.close()
    }
  }

  //////////////////////////////////
  // Collections.scala
  //////////////////////////////////

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
   * Iterator.grouped(7).map(_.par.map(_.apply).seq).next
   */
  implicit class ChunkingParIterator[A](val self: Iterator[() => A]) extends AnyVal {
    def parChunked(chunkSize: Int) = new Iterator[A] {
      var currentChunk: Iterator[A] = Iterator.empty

      def next() = {
        if (!currentChunk.hasNext) {
          val v = (Array.newBuilder[() => A] ++= (for (_ <- 0 until chunkSize if self.hasNext) yield self.next())).result
          currentChunk = v.par.map(_.apply).seq.iterator
        }
        currentChunk.next()
      }
      def hasNext() = currentChunk.hasNext || self.hasNext
    }
  }

  //  /**
  //   *
  //   */
  //  class KMaxPriorityQueue[A](k: Int)(implicit ord: scala.math.Ordering[A]) {
  //    private[this] val q = collection.mutable.PriorityQueue.empty[A](ord.reverse)
  //    def +=(e: A) = { q += e; balance(); this }
  //    private[this] def balance(): Unit = { while (q.length > k) q.dequeue }
  //    def iterator = toVector.iterator
  //    def toVector = (collection.mutable.PriorityQueue.empty[A](ord) ++= q.iterator).dequeueAll.toVector
  //    override def toString = f"KMaxPriorityQueue(k)(${q})"
  //  }
  //  object KMaxPriorityQueue {
  //    def empty[A](k: Int)(implicit ord: scala.math.Ordering[A]) = new KMaxPriorityQueue(k)(ord)
  //  }

  /**
   * Data structure that moves an arbitrarily growing/shrinking window over
   * an iterator, preserving the underlying iterator for future method calls.
   */
  class WindowIteratorish[A](stuff: Iterator[A]) {
    def visible: Vector[A] = window.toVector
    private[this] val window = mutable.Queue[A]()
    private[this] var itr = stuff

    def advanceFrontWhile(p: A => Boolean): WindowIteratorish[A] = {
      @tailrec def inner() {
        if (itr.hasNext) {
          val a = itr.next()
          if (p(a)) {
            window += a
            inner()
          }
          else {
            itr = a +: itr
          }
        }
      }
      inner()
      this
    }

    def advanceRearWhile(p: A => Boolean): WindowIteratorish[A] = {
      while (window.nonEmpty && p(window.front))
        window.dequeue()
      this
    }
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

  //////////////////////////////////////////////////////
  // toTuple2: (T,T)
  // toTuple3: (T,T,T)
  // toTuple4: (T,T,T,T)
  // toTuple5: (T,T,T,T,T)
  //   - Convert this sequence to a tuple
  //////////////////////////////////////////////////////

  final implicit class Enriched_toTuple_Seq[A](val seq: Seq[A]) extends AnyVal {
    def toTuple2 = seq match { case Seq(a, b) => (a, b); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple2: $x") }
    def toTuple3 = seq match { case Seq(a, b, c) => (a, b, c); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple3: $x") }
    def toTuple4 = seq match { case Seq(a, b, c, d) => (a, b, c, d); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple4: $x") }
    def toTuple5 = seq match { case Seq(a, b, c, d, e) => (a, b, c, d, e); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple5: $x") }
  }

  final implicit class Enriched_toTuple_Array[A](val seq: Array[A]) extends AnyVal {
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

  final implicit class Enriched_prependAppend_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Prepend an item to the front of the iterator
     *
     * @param elem  the item to be prepended
     * @return a new iterator
     */
    def +:[B >: A](elem: B): Iterator[B] =
      Iterator(elem) ++ self

    /**
     * Append an item to the end of the iterator
     *
     * @param elem  the item to be appended
     * @return a new iterator
     */
    def :+[B >: A](elem: B): Iterator[B] =
      self ++ Iterator(elem)
  }

  //////////////////////////////////////////////////////
  // counts(): Map[A, Int]
  //   - Map each distinct item in the collection to the number of times it appears.
  //////////////////////////////////////////////////////

  final implicit class Enriched_counts_TraversableOnce[A](val self: TraversableOnce[A]) extends AnyVal {
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

  final implicit class Enriched_groupBy_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Same functionality as Traversable.groupBy(f)
     *
     * @param f function mapping items to new keys
     * @return Map from new keys to original items
     */
    def groupBy[K](f: A => K): Map[K, Vector[A]] =
      this.groupBy(f, Vector.newBuilder[A])

    /**
     * Same functionality as Traversable.groupBy(f)
     *
     * @param f function mapping items to new keys
     * @param builder a builder to construct collections of items that have been grouped
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

  final implicit class Enriched_groupByKey_Traversable[K, V, Repr](val self: TraversableLike[(K, V), Repr]) extends AnyVal {
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

  final implicit class Enriched_groupByKey_Iterator[A](val self: Iterator[A]) extends AnyVal {
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

  final implicit class Enriched_ungroup_GenTraversableOnce[A, B](val self: GenTraversableOnce[(A, GenTraversableOnce[B])]) extends AnyVal {
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
  // groupedAsVector(n: Int): Iterator[Vector[A]]
  //   - same as Iterator.grouped(n).map(_.toVector)
  //////////////////////////////////////////////////////

  implicit class GroupedAsVectorIterator[A](val self: Iterator[A]) {
    def groupedAsVector(n: Int): Iterator[Vector[A]] = new Iterator[Vector[A]] {
      def next() = self.nextN(n)
      def hasNext = self.hasNext
    }
  }

  //////////////////////////////////////////////////////
  // nextN(n: Int): Vector[A]
  //   - sort of like Vector.fill(n)(itr.next), but never calls `next` on empty
  //////////////////////////////////////////////////////

  implicit class NextNIterator[A](val self: Iterator[A]) {
    def nextN(n: Int): Vector[A] = {
      (Vector.newBuilder[A] ++= (for (_ <- 1 to n if self.hasNext) yield self.next())).result
    }
  }

  //////////////////////////////////////////////////////
  // dropRightWhile(p: A => Boolean): Repr
  //////////////////////////////////////////////////////

  final implicit class Enriched_dropRightWhile_Seq[A, Repr](val self: SeqLike[A, Repr]) extends AnyVal {
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

  final implicit class Enriched_dropRightWhile_String(val self: String) extends AnyVal {
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

  class Counter(startAt: Int = 0) { private[this] var i = startAt; def get = i; def inc() = { i += 1; this }; override def toString = f"Counter($i)" }
  private[this] class Enriched_splitAt_Iterator_FirstItr[A](self: Iterator[A], n: Int, c: Counter) extends Iterator[A] {
    def next(): A = {
      assert(hasNext, "first has already been read completely")
      c.inc(); self.next
    }
    def hasNext() = c.get < n && self.hasNext
  }
  private[this] class Enriched_splitAt_Iterator_SecondItr[A](self: Iterator[A], n: Int, c: Counter) extends Iterator[A] {
    def next(): A = {
      assert(c.get >= n, "first has NOT YET been read completely")
      assert(hasNext, "second has already been read completely")
      c.inc(); self.next
    }
    def hasNext() = self.hasNext
  }
  final implicit class Enriched_splitAt_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Safely split this iterator at the specified index.  The 'first'
     * iterator must be exhausted completely before the items in the 'second'
     * iterator can be accessed.
     *
     * Inspired by Traversable.splitAt
     *
     * @param n The index at which to split the collection
     * @return  a pair: the items before the split point and the items
     *          starting with the split point
     */
    def splitAt(n: Int): (Iterator[A], Iterator[A]) = {
      val c = new Counter()
      val first: Iterator[A] = new Enriched_splitAt_Iterator_FirstItr(self, n, c)
      val second: Iterator[A] = new Enriched_splitAt_Iterator_SecondItr(self, n, c)
      (first, second)
    }
  }

  /**
   * The KeepDelimiter enumeration is used to specify behavior for the `split` methods.
   */
  sealed trait KeepDelimiter
  case object DropDelimiter extends KeepDelimiter
  case object KeepDelimiterAsFirst extends KeepDelimiter
  case object KeepDelimiterAsLast extends KeepDelimiter

  //////////////////////////////////////////////////////
  // split(delim: A): Iterator[Repr[A]]
  //   - Split this collection on each occurrence of the delimiter 
  //   - Inspired by String.split
  //////////////////////////////////////////////////////

  final implicit class Enriched_split_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Split this collection on each occurrence of the delimiter.
     *
     * Inspired by String.split
     *
     * @param delim The delimiter upon which to split.
     */
    def split(delim: A, keepDelimiter: KeepDelimiter = DropDelimiter): Iterator[Vector[A]] =
      split(delim, Vector.newBuilder[A], keepDelimiter)

    /**
     * Split this collection on each occurrence of the delimiter.
     *
     * Inspired by String.split
     *
     * @param delim The delimiter upon which to split.
     */
    def split[That](delim: A, builder: => Builder[A, That]): Iterator[That] =
      self.splitWhere(_ == delim, builder, DropDelimiter)

    /**
     * Split this collection on each occurrence of the delimiter.
     *
     * Inspired by String.split
     *
     * @param delim The delimiter upon which to split.
     */
    def split[That](delim: A, builder: => Builder[A, That], keepDelimiter: KeepDelimiter): Iterator[That] =
      self.splitWhere(_ == delim, builder, keepDelimiter)
  }

  final implicit class Enriched_split_Traversable[A, Repr](val self: TraversableLike[A, Repr]) extends AnyVal {
    /**
     * Split this collection on each occurrence of the delimiter.
     *
     * Inspired by String.split
     *
     * @param delim The delimiter upon which to split.
     */
    def split[That](delim: A, keepDelimiter: KeepDelimiter = DropDelimiter)(implicit bf: CanBuildFrom[Repr, A, That]): Iterator[That] =
      self.toIterator.split(delim, bf(self.asInstanceOf[Repr]), keepDelimiter)
  }

  //////////////////////////////////////////////////////
  // splitWhere(p: A => Boolean): Iterator[Repr[A]]
  //   - Split this on items for which the predicate is true 
  //////////////////////////////////////////////////////

  private[this] class SplitWhereIterator[A, That](self: Iterator[A], p: A => Boolean, builder: => Builder[A, That], keepDelimiter: KeepDelimiter) extends Iterator[That] {
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
          if (keepDelimiter == KeepDelimiterAsLast) {
            bldr += x
          }
          queued = Some(bldr.result)
          bldr.clear()
          if (keepDelimiter == KeepDelimiterAsFirst) {
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
  final implicit class Enriched_splitWhere_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Split this on items for which the predicate is true.
     *
     * @param delim The delimiter upon which to split.
     */
    def splitWhere(p: A => Boolean, keepDelimiter: KeepDelimiter = DropDelimiter): Iterator[Vector[A]] =
      splitWhere(p, Vector.newBuilder[A], keepDelimiter)

    /**
     * Split this on items for which the predicate is true.
     *
     * @param delim The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean, builder: => Builder[A, That]): Iterator[That] =
      splitWhere(p, builder, DropDelimiter)

    /**
     * Split this on items for which the predicate is true.
     *
     * @param delim The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean, builder: => Builder[A, That], keepDelimiter: KeepDelimiter): Iterator[That] =
      new SplitWhereIterator(self, p, builder, keepDelimiter)
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

  final implicit class Enriched_splitWhere_Traversable[A, Repr](val self: TraversableLike[A, Repr]) extends AnyVal {
    /**
     * Split this on items for which the predicate is true.  Delimiters
     * do not appear in the output.
     *
     * @param delim The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean, keepDelimiter: KeepDelimiter = DropDelimiter)(implicit bf: CanBuildFrom[Repr, A, That]): Iterator[That] =
      self.toIterator.splitWhere(p, bf(self.asInstanceOf[Repr]), keepDelimiter)
  }

  //////////////////////////////////////////////////////
  // zipSafe(that: GenTraversable[B]): Repr[(A,B)]
  //   - zip this collection with another, throwing an exception if they are
  //     not of equal length.
  //////////////////////////////////////////////////////

  private[this] class ZipSafeIterator[A, B](self: Iterator[A], thatItr: Iterator[B]) extends Iterator[(A, B)] {
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
  final implicit class Enriched_zipSafe_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that  the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException thrown if collections differ in length
     */
    def zipSafe[B](that: GenTraversableOnce[B]): Iterator[(A, B)] = {
      val thatItr = that.toIterator
      new ZipSafeIterator(self, thatItr)
    }
  }

  final implicit class Enriched_zipSafe_GenTraversable[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that  the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException thrown if collections differ in length
     */
    def zipSafe[A1 >: A, B, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      b ++= (self.toIterator zipSafe that)
      b.result
    }
  }

  final implicit class Enriched_zipSafe_Array[A](val self: Array[A]) extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that  the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException thrown if collections differ in length
     */
    def zipSafe[B, That](that: GenTraversableOnce[B]): Array[(A, B)] = {
      val b = ArrayBuilder.make[(A, B)]()
      b.sizeHint(self.size)
      b ++= (self.toIterator zipSafe that)
      b.result
    }
  }

  final implicit class Enriched_zipSafe_Tuple_of_Iterator[A, B](val self: (Iterator[A], GenTraversableOnce[B])) extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @return an iterator of pairs
     * @throws RuntimeException thrown if collections differ in length
     */
    def zipSafe = self._1 zipSafe self._2
  }

  final implicit class Enriched_zipSafe_Tuple_of_GenTraversable[A, Repr, B](val self: (GenTraversableLike[A, Repr], GenTraversableOnce[B])) extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that  the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException thrown if collections differ in length
     */
    def zipSafe[A1 >: A, That](implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(self._1.asInstanceOf[Repr])
      b ++= (self._1.toIterator zipSafe self._2)
      b.result
    }
  }

  def zipSafe[A, Repr, B, A1 >: A, That](a: GenTraversableLike[A, Repr], b: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
    val ai = a.toIterator
    val bi = b.toIterator
    val bldr = bf(a.asInstanceOf[Repr])
    while (ai.hasNext && bi.hasNext) bldr += ((ai.next, bi.next))
    assert(!ai.hasNext && !bi.hasNext, s"a=${if (ai.hasNext) "nonempty" else "empty"} b=${if (bi.hasNext) "nonempty" else "empty"}")
    bldr.result
  }

  def zipSafe[A, Repr, B, C, A1 >: A, That](a: GenTraversableLike[A, Repr], b: GenTraversableOnce[B], c: GenTraversableOnce[C])(implicit bf: CanBuildFrom[Repr, (A1, B, C), That]): That = {
    val ai = a.toIterator
    val bi = b.toIterator
    val ci = c.toIterator
    val bldr = bf(a.asInstanceOf[Repr])
    while (ai.hasNext && bi.hasNext && ci.hasNext) bldr += ((ai.next, bi.next, ci.next))
    assert(!ai.hasNext && !bi.hasNext && !ci.hasNext, s"a=${if (ai.hasNext) "nonempty" else "empty"} b=${if (bi.hasNext) "nonempty" else "empty"} c=${if (ci.hasNext) "nonempty" else "empty"}")
    bldr.result
  }

  def zipSafe[A, Repr, B, C, D, A1 >: A, That](a: GenTraversableLike[A, Repr], b: GenTraversableOnce[B], c: GenTraversableOnce[C], d: GenTraversableOnce[D])(implicit bf: CanBuildFrom[Repr, (A1, B, C, D), That]): That = {
    val ai = a.toIterator
    val bi = b.toIterator
    val ci = c.toIterator
    val di = d.toIterator
    val bldr = bf(a.asInstanceOf[Repr])
    while (ai.hasNext && bi.hasNext && ci.hasNext && di.hasNext) bldr += ((ai.next, bi.next, ci.next, di.next))
    assert(!ai.hasNext && !bi.hasNext && !ci.hasNext && !di.hasNext, s"a=${if (ai.hasNext) "nonempty" else "empty"} b=${if (bi.hasNext) "nonempty" else "empty"} c=${if (ci.hasNext) "nonempty" else "empty"} d=${if (di.hasNext) "nonempty" else "empty"}")
    bldr.result
  }

  //////////////////////////////////////////////////////
  // unzip 
  //   - Unzip this iterator of pairs into two iterators.
  //   - The new iterators coordinate to maintain laziness.
  //////////////////////////////////////////////////////

  final implicit class Enriched_unzip3_GenTraversable[A, B, C, D, Repr <: GenTraversable[(A, B, C, D)]](val self: GenTraversableLike[(A, B, C, D), Repr]) extends AnyVal {
    def unzip4[ThatA, ThatB, ThatC, ThatD](implicit // 
    bfA: CanBuildFrom[Repr, A, ThatA],
      bfB: CanBuildFrom[Repr, B, ThatB],
      bfC: CanBuildFrom[Repr, C, ThatC],
      bfD: CanBuildFrom[Repr, D, ThatD] //
      ): (ThatA, ThatB, ThatC, ThatD) = {
      val bldrA = bfA(self.asInstanceOf[Repr])
      val bldrB = bfB(self.asInstanceOf[Repr])
      val bldrC = bfC(self.asInstanceOf[Repr])
      val bldrD = bfD(self.asInstanceOf[Repr])
      for ((a, b, c, d) <- self.seq) {
        bldrA += a
        bldrB += b
        bldrC += c
        bldrD += d
      }
      (bldrA.result(), bldrB.result(), bldrC.result(), bldrD.result())
    }
  }

  final implicit class Enriched_unzip5_GenTraversable[A, B, C, D, E, Repr <: GenTraversable[(A, B, C, D, E)]](val self: GenTraversableLike[(A, B, C, D, E), Repr]) extends AnyVal {
    def unzip5[ThatA, ThatB, ThatC, ThatD, ThatE](implicit // 
    bfA: CanBuildFrom[Repr, A, ThatA],
      bfB: CanBuildFrom[Repr, B, ThatB],
      bfC: CanBuildFrom[Repr, C, ThatC],
      bfD: CanBuildFrom[Repr, D, ThatD],
      bfE: CanBuildFrom[Repr, E, ThatE] //
      ): (ThatA, ThatB, ThatC, ThatD, ThatE) = {
      val bldrA = bfA(self.asInstanceOf[Repr])
      val bldrB = bfB(self.asInstanceOf[Repr])
      val bldrC = bfC(self.asInstanceOf[Repr])
      val bldrD = bfD(self.asInstanceOf[Repr])
      val bldrE = bfE(self.asInstanceOf[Repr])
      for ((a, b, c, d, e) <- self.seq) {
        bldrA += a
        bldrB += b
        bldrC += c
        bldrD += d
        bldrE += e
      }
      (bldrA.result(), bldrB.result(), bldrC.result(), bldrD.result(), bldrE.result())
    }
  }

  final implicit class Enriched_unzip6_GenTraversable[A, B, C, D, E, F, Repr <: GenTraversable[(A, B, C, D, E, F)]](val self: GenTraversableLike[(A, B, C, D, E, F), Repr]) extends AnyVal {
    def unzip6[ThatA, ThatB, ThatC, ThatD, ThatE, ThatF](implicit // 
    bfA: CanBuildFrom[Repr, A, ThatA],
      bfB: CanBuildFrom[Repr, B, ThatB],
      bfC: CanBuildFrom[Repr, C, ThatC],
      bfD: CanBuildFrom[Repr, D, ThatD],
      bfE: CanBuildFrom[Repr, E, ThatE],
      bfF: CanBuildFrom[Repr, F, ThatF] //
      ): (ThatA, ThatB, ThatC, ThatD, ThatE, ThatF) = {
      val bldrA = bfA(self.asInstanceOf[Repr])
      val bldrB = bfB(self.asInstanceOf[Repr])
      val bldrC = bfC(self.asInstanceOf[Repr])
      val bldrD = bfD(self.asInstanceOf[Repr])
      val bldrE = bfE(self.asInstanceOf[Repr])
      val bldrF = bfF(self.asInstanceOf[Repr])
      for ((a, b, c, d, e, f) <- self.seq) {
        bldrA += a
        bldrB += b
        bldrC += c
        bldrD += d
        bldrE += e
        bldrF += f
      }
      (bldrA.result(), bldrB.result(), bldrC.result(), bldrD.result(), bldrE.result(), bldrF.result())
    }
  }

  private[this] abstract class QueuedPairIterator[A, B, T, O](self: Iterator[(A, B)], thisQueue: mutable.Queue[T], otherQueue: mutable.Queue[O]) extends Iterator[T] {
    protected[this] def swapOrNot(p: (A, B)): (T, O)
    override def hasNext = thisQueue.nonEmpty || self.hasNext
    override def next =
      if (thisQueue.nonEmpty) thisQueue.dequeue()
      else { val (t, o) = swapOrNot(self.next()); otherQueue.enqueue(o); t }
  }
  private[this] class SwappingQueuedPairIterator[A, B](self: Iterator[(A, B)], aQueue: mutable.Queue[A], bQueue: mutable.Queue[B]) extends QueuedPairIterator[A, B, A, B](self, aQueue, bQueue) { override def swapOrNot(p: (A, B)) = p }
  private[this] class NonSwpngQueuedPairIterator[A, B](self: Iterator[(A, B)], aQueue: mutable.Queue[A], bQueue: mutable.Queue[B]) extends QueuedPairIterator[A, B, B, A](self, bQueue, aQueue) { override def swapOrNot(p: (A, B)) = p.swap }
  final implicit class Enriched_unzip2_Iterator[A, B](val self: Iterator[(A, B)]) extends AnyVal {
    def unzip(): (Iterator[A], Iterator[B]) = {
      val aQueue = mutable.Queue[A]()
      val bQueue = mutable.Queue[B]()
      val aItr = new SwappingQueuedPairIterator(self, aQueue, bQueue)
      val bItr = new NonSwpngQueuedPairIterator(self, aQueue, bQueue)
      (aItr, bItr)
    }
  }

  //////////////////////////////////////////////////////
  // flatCollect[B](pf: A => B): Repr[B]
  //   - Functionally equivalent to:
  //         collect(x => x -> f(x)).flatten
  //////////////////////////////////////////////////////

  final implicit class Enriched_flatCollect_TraversableLike[A, Repr](val self: TraversableLike[A, Repr]) extends AnyVal {
    /**
     * Functionally equivalent to: collect(x => x -> f(x)).flatten
     */
    def flatCollect[B, That](pf: PartialFunction[A, GenTraversableOnce[B]])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
      def builder = bf(self.asInstanceOf[Repr]) // extracted to keep method size under 35 bytes, so that it can be JIT-inlined
      val b = builder
      self.foreach(pf.runWith(b ++= _.seq))
      b.result
    }
  }

  final implicit class Enriched_flatCollect_Iterator[A](val itr: Iterator[A]) extends AnyVal {
    /**
     * Functionally equivalent to: collect(x => x -> f(x)).flatten
     */
    def flatCollect[B](pf: PartialFunction[A, GenTraversableOnce[B]]): Iterator[B] = new scala.collection.AbstractIterator[B] {
      private var cur: Iterator[B] = Iterator.empty
      val self = itr.buffered
      private def skip() = while (self.hasNext && !pf.isDefinedAt(self.head)) self.next()
      def hasNext = { skip(); cur.hasNext || self.hasNext && { cur = pf(self.next()).toIterator; hasNext } }
      def next(): B = { skip(); (if (hasNext) cur else Iterator.empty).next() }
    }
  }

  //////////////////////////////////////////////////////
  // mapTo[B](f: A => B): Repr[(A,B)]
  //   - Map a function over the collection, returning a set of pairs consisting 
  //     of the original item and the result of the function application
  //   - Functionally equivalent to:
  //         map(x => x -> f(x))
  //////////////////////////////////////////////////////

  final implicit class Enriched_mapTo_GenTraversableLike[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Map a function over the collection, returning a set of pairs consisting
     * of the original item and the result of the function application
     *
     * Functionally equivalent to: map(x => x -> f(x))
     *
     * @param f the function to map
     * @return the new collection
     */
    def mapTo[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, (A, B), That]): That = {
      self.map(x => x -> f(x))
    }
  }

  private[this] class MapToIterator[A, B](self: Iterator[A], f: A => B) extends Iterator[(A, B)] {
    def hasNext = self.hasNext
    def next() = {
      val x = self.next
      x -> f(x)
    }
  }
  final implicit class Enriched_mapTo_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Map a function over the collection, returning a set of pairs consisting
     * of the original item and the result of the function application
     *
     * Functionally equivalent to: map(x => x -> f(x))
     *
     * @param f the function to map
     * @return a new iterator
     */
    def mapTo[B](f: A => B): Iterator[(A, B)] = new MapToIterator(self, f)
  }

  //////////////////////////////////////////////////////
  // mapToVal[B](v: B): Repr[(A,B)]
  //   - Map each item in the collection to a particular value
  //   - Functionally equivalent to:
  //         map(x => x -> v)
  //////////////////////////////////////////////////////

  final implicit class Enriched_mapToVal_GenTraversableLike[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Map each item in the collection to a particular value
     *
     * Functionally equivalent to: map(x => x -> v)
     *
     * @param v the value to map to
     * @return the new collection
     */
    def mapToVal[B, That](v: => B)(implicit bf: CanBuildFrom[Repr, (A, B), That]): That = {
      self.map(_ -> v)
    }
  }

  private[this] class MapToValIterator[A, B](self: Iterator[A], v: => B) extends Iterator[(A, B)] {
    def hasNext = self.hasNext
    def next() = self.next -> v
  }
  final implicit class Enriched_mapToVal_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Map each item in the collection to a particular value
     *
     * Functionally equivalent to: map(x => x -> v)
     *
     * @param v the value to map to
     * @return a new iterator
     */
    def mapToVal[B](v: => B): Iterator[(A, B)] = new MapToValIterator(self, v)
  }

  //////////////////////////////////////////////////////
  // mapKeys(f: T => R): Repr[(R,U)]
  //   - In a collection of pairs, map a function over the first item of each pair.
  //   - Functionally equivalent to:
  //         this.map{case (k,v) => f(k) -> v}
  //////////////////////////////////////////////////////

  final implicit class Enriched_mapKeys_GenTraversable[T, U, Repr](val self: GenTraversableLike[(T, U), Repr]) extends AnyVal {
    /**
     * In a collection of pairs, map a function over the first item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f function to map over the first item of each pair
     * @return a collection of pairs
     */
    def mapKeys[R, That](f: T => R)(implicit bf: CanBuildFrom[Repr, (R, U), That]) = {
      self.map(x => f(x._1) -> x._2)
    }
  }

  private[this] class MapKeysIterator[T, U, R](self: Iterator[(T, U)], f: T => R) extends Iterator[(R, U)] {
    def hasNext = self.hasNext
    def next() = {
      val (k, v) = self.next()
      f(k) -> v
    }
  }
  final implicit class Enriched_mapKeys_Iterator[T, U](val self: Iterator[(T, U)]) extends AnyVal {
    /**
     * In a collection of pairs, map a function over the first item of each
     * pair.
     *
     * @param f function to map over the first item of each pair
     * @return a collection of pairs
     */
    def mapKeys[R](f: T => R): Iterator[(R, U)] = new MapKeysIterator(self, f)
  }

  //////////////////////////////////////////////////////
  // mapVals(f: U => R): Repr[(T,R)]
  //   - In a collection of pairs, map a function over the second item of each pair.
  //   - Ensures that the map is computed at call-time, and not returned as a view as `Map.mapValues` would do.
  //   - Equivalent to: this.map { case (k,v) => k -> f(v) }
  //////////////////////////////////////////////////////

  final implicit class Enriched_mapVals_GenTraversable[T, U, Repr](val self: GenTraversableLike[(T, U), Repr]) extends AnyVal {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapVals[R, That](f: U => R)(implicit bf: CanBuildFrom[Repr, (T, R), That]) = {
      self.map(x => x._1 -> f(x._2))
    }
  }

  private[this] class MapValsIterator[T, U, R](self: Iterator[(T, U)], f: U => R) extends Iterator[(T, R)] {
    def hasNext = self.hasNext
    def next() = {
      val (k, v) = self.next()
      k -> f(v)
    }
  }
  final implicit class Enriched_mapVals_Iterator[T, U](val self: Iterator[(T, U)]) extends AnyVal {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.
     *
     * @param f function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapVals[R](f: U => R): Iterator[(T, R)] = new MapValsIterator(self, f)
  }

  //////////////////////////////////////////////////////
  // submap(f: T => R): Repr[R]
  //   - In a collection of collections, map a function over the inner collections without flattening.
  //   - Equivalent to: this.map(_.map(x => f(x)))
  //////////////////////////////////////////////////////

  final implicit class Enriched_submap_GenTraversable[T, TRepr, SRepr](val self: GenTraversableLike[GenTraversableLike[T, TRepr], SRepr]) extends AnyVal {
    /**
     * In a collection of collections, map a function over the inner collections without flattening.
     *
     * @param f function to map over the inner collections
     * @return a collection of collections
     */
    def submap[R, TThat, SThat](f: T => R)(implicit tbf: CanBuildFrom[TRepr, R, TThat], bf: CanBuildFrom[SRepr, TThat, SThat]) = {
      self.map(s => s.map(f)(tbf))
    }
  }

  private[this] class SubmapIterator[T, R, Repr, That](self: Iterator[GenTraversableLike[T, Repr]], f: T => R, bf: CanBuildFrom[Repr, R, That]) extends Iterator[That] {
    def hasNext = self.hasNext
    def next() = self.next().map(f)(bf)
  }
  final implicit class Enriched_submap_Iterator[T, Repr](val self: Iterator[GenTraversableLike[T, Repr]]) extends AnyVal {
    /**
     * In a collection of collections, map a function over the inner collections without flattening.
     *
     * @param f function to map over the inner collections
     * @return a collection of collections
     */
    def submap[R, That](f: T => R)(implicit bf: CanBuildFrom[Repr, R, That]): Iterator[That] = new SubmapIterator(self, f, bf)
  }

  //////////////////////////////////////////////////////
  // mapt[A,B,R](f: (A,B) => R): Repr[R]
  //   - map over a Tuple2
  //   - same as `xs.map { case (x,y) => f(x,y) } `
  //////////////////////////////////////////////////////

  final implicit class Enriched_mapt_2_GenTraversableLike[A, B, Repr](val self: GenTraversableLike[(A, B), Repr]) extends AnyVal {
    def mapt[R, That](f: (A, B) => R)(implicit bf: CanBuildFrom[Repr, R, That]) = {
      self.map(x => f(x._1, x._2))
    }
  }

  private[this] class Mapt2Iterator[A, B, R](self: Iterator[(A, B)], f: (A, B) => R) extends Iterator[R] {
    def next() = {
      val x = self.next
      f(x._1, x._2)
    }
    def hasNext() = self.hasNext
  }
  final implicit class Enriched_mapt_2_Iterator[A, B](val self: Iterator[(A, B)]) extends AnyVal {
    def mapt[R](f: (A, B) => R): Iterator[R] = new Mapt2Iterator(self, f)
  }

  final implicit class Enriched_mapt_3_GenTraversableLike[A, B, C, Repr](val self: GenTraversableLike[(A, B, C), Repr]) extends AnyVal {
    def mapt[R, That](f: (A, B, C) => R)(implicit bf: CanBuildFrom[Repr, R, That]) = {
      self.map(x => f(x._1, x._2, x._3))
    }
  }

  private[this] class Mapt3Iterator[A, B, C, R](self: Iterator[(A, B, C)], f: (A, B, C) => R) extends Iterator[R] {
    def next() = {
      val x = self.next
      f(x._1, x._2, x._3)
    }
    def hasNext() = self.hasNext
  }
  final implicit class Enriched_mapt_3_Iterator[A, B, C](val self: Iterator[(A, B, C)]) extends AnyVal {
    def mapt[R](f: (A, B, C) => R): Iterator[R] = new Mapt3Iterator(self, f)
  }

  final implicit class Enriched_mapt_4_GenTraversableLike[A, B, C, D, Repr](val self: GenTraversableLike[(A, B, C, D), Repr]) extends AnyVal {
    def mapt[R, That](f: (A, B, C, D) => R)(implicit bf: CanBuildFrom[Repr, R, That]) = {
      self.map(x => f(x._1, x._2, x._3, x._4))
    }
  }

  private[this] class Mapt4Iterator[A, B, C, D, R](self: Iterator[(A, B, C, D)], f: (A, B, C, D) => R) extends Iterator[R] {
    def next() = {
      val x = self.next
      f(x._1, x._2, x._3, x._4)
    }
    def hasNext() = self.hasNext
  }
  final implicit class Enriched_mapt_4_Iterator[A, B, C, D](val self: Iterator[(A, B, C, D)]) extends AnyVal {
    def mapt[R](f: (A, B, C, D) => R): Iterator[R] = new Mapt4Iterator(self, f)
  }

  //////////////////////////////////////////////////////
  // foldLeftWhile[A,B](z: B)(p: (B, A) => Boolean)(op: (B, A) => B): B
  //   - Folds while the condition `p` is true.
  //   - If `p` operates on the item, then it behaves like `takeWhile`;
  //     if it operates on the accumulator, then it behaves like `while`.
  //////////////////////////////////////////////////////

  final implicit class Enriched_foldLeftWhile_GenTraversableOnce[A](val self: GenTraversableOnce[A]) extends AnyVal {
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

  final implicit class Enrich_avg_GenTraversableOnce[A](val self: GenTraversableOnce[A])(implicit num: Fractional[A]) {
    /**
     * Find the average (mean) of this collection of numbers.
     *
     * @return the average (mean)
     */
    def avg: A = {
      assert(self.nonEmpty, "cannot average an empty collection")
      val (total, count) = self.foldLeft((num.zero, num.zero)) {
        case ((total, count), x) => (num.plus(total, x), num.plus(count, num.one))
      }
      num.div(total, count)
    }
    def mean: A = avg
  }

  final implicit class Enrich_avg_Int_GenTraversableOnce(val self: GenTraversableOnce[Int]) extends AnyVal {
    /**
     * Find the average (mean) of this collection of numbers.
     *
     * @return the average (mean)
     */
    def avg: Double = {
      assert(self.nonEmpty, "cannot average an empty collection")
      val (total, count) = self.foldLeft((0, 0)) {
        case ((total, count), x) => (total + x, count + 1)
      }
      total.toDouble / count
    }
    def mean: Double = avg
  }

  //////////////////////////////////////////////////////
  // stdev(): A
  //   - Find the standard deviation of this collection of numbers
  //////////////////////////////////////////////////////

  final implicit class Enrich_stdev_GenTraversableOnce[A](val self: GenTraversableOnce[A])(implicit num: Fractional[A]) {
    /**
     * Find the standard deviation of this collection of numbers.
     *
     * @return the standard deviation
     */
    def stdev: Double = {
      val vec: Vector[Double] = self.toIterator.map(num.toDouble).toVector
      val m = vec.mean.toDouble
      sqrt(vec.map(x => pow(m - x, 2)).mean)
    }
  }

  final implicit class Enrich_stdev_Int_GenTraversableOnce(val self: GenTraversableOnce[Int]) extends AnyVal {
    /**
     * Find the standard deviation of this collection of numbers.
     *
     * @return the standard deviation
     */
    def stdev: Double = {
      val vec = self.toIterator.toVector
      val m = vec.mean.toDouble
      sqrt(vec.map(x => pow(m - x, 2)).mean)
    }
  }

  //////////////////////////////////////////////////////
  // proportion(p: A => Boolean): Double
  //   - Find the proportion of elements for which the predicate holds
  //////////////////////////////////////////////////////

  final implicit class Enrich_proportion_GenTraversableOnce[A](val self: GenTraversableOnce[A]) {
    /**
     * Find the proportion of elements for which the predicate holds
     *
     * @return the proportion
     */
    def proportion(p: A => Boolean) = {
      assert(self.nonEmpty, "cannot call `proportion` on an empty collection")
      val (total, count) = self.foldLeft((0, 0)) {
        case ((total, count), x) => (total + 1, if (p(x)) count + 1 else count)
      }
      count / total.toDouble
    }
  }

  //////////////////////////////////////////////////////
  // normalize(): Repr[A]
  //   - Normalize this collection of numbers by dividing each by the sum
  //////////////////////////////////////////////////////

  final implicit class Enriched_normalize_GenTraversable[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
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

  final implicit class Enriched_normalize_Int_GenTraversable[Repr](val self: GenTraversableLike[Int, Repr]) extends AnyVal {
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

  final implicit class Enriched_normalizeValues_GenTraversable[T, U, Repr](val self: GenTraversableLike[(T, U), Repr]) extends AnyVal {
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

  final implicit class Enriched_normalizeValues_Int_GenTraversable[T, Repr](val self: GenTraversableLike[(T, Int), Repr]) extends AnyVal {
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
  // maxByN(n: Int)(f: A => B): Repr[A]
  //   - Equivalent to, but faster than self.sortBy(f).takeRight(n).reverse
  //////////////////////////////////////////////////////

  final implicit class Enriched_maxByN_GenTraversable[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Find the N maximum entries in the collection.
     *
     * @return a collection containing the N maximum entries, sorted so the max is first
     */
    def maxByN[B, That](n: Int)(f: A => B)(implicit ord: scala.math.Ordering[B], bf: CanBuildFrom[Repr, A, That]): That = {
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

  final implicit class Enriched_maxByN_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Find the N maximum entries in the collection.
     *
     * @return a collection containing the N maximum entries, sorted so the max is first
     */
    def maxByN[B](n: Int)(f: A => B)(implicit ord: scala.math.Ordering[B]): Vector[A] = {
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
  // minByN(n: Int)(f: A => B): Repr[A]
  //   - Equivalent to, but faster than self.sortBy(f).take(n)
  //////////////////////////////////////////////////////

  final implicit class Enriched_minByN_GenTraversable[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Find the N minimum entries in the collection.
     *
     * @return a collection containing the N minimum entries, sorted so the min is first
     */
    def minByN[B, That](n: Int)(f: A => B)(implicit ord: scala.math.Ordering[B], bf: CanBuildFrom[Repr, A, That]): That = {
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

  final implicit class Enriched_minByN_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Find the N minimum entries in the collection.
     *
     * @return a collection containing the N minimum entries, sorted so the min is first
     */
    def minByN[B](n: Int)(f: A => B)(implicit ord: scala.math.Ordering[B]): Vector[A] = {
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
  // distinctBy[A, B](f: A => B): Seq[A]
  //   - Remove duplicates according to some function `f`.
  //////////////////////////////////////////////////////

  final implicit class Enriched_distinctBy_GenSeqLike[A, Repr](val self: GenSeqLike[A, Repr]) extends AnyVal {
    /**
     * Remove duplicates according to some function `f`.
     *
     * @param p the function to determine the duplication key
     * @return the new collection
     */
    def distinctBy[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      val builder = bf()
      val seen = mutable.HashSet[B]()
      for (a <- self) {
        val b = f(a)
        if (!seen(b)) {
          builder += a
          seen += b
        }
      }
      builder.result()
    }
  }

  //////////////////////////////////////////////////////
  // sumBy[B: Numeric](f: A => B): B
  //   - Map a numeric-producing function over each item and sum the results 
  //   - Functionally equivalent to:
  //         this.map(f).sum
  //////////////////////////////////////////////////////

  final implicit class Enriched_sumBy_GenTraversableOnce[A](val self: GenTraversableOnce[A]) extends AnyVal {
    /**
     * Map a numeric-producing function over each item and sum the results.
     *
     * Functionally equivalent to `this.map(f).sum`
     *
     * @param f A function that produces a Numeric
     * @return the sum of the results after applications of f
     */
    def sumBy[B](f: A => B)(implicit num: Numeric[B]): B = {
      (num.zero /: self)((accum, x) => num.plus(accum, f(x)))
    }
  }

  //////////////////////////////////////////////////////
  // sliding2: Iterator[(A,A)]
  //   - slide over this collection to produce pairs.
  //   - Functionally equivalent to:
  //         this.sliding(2).map{Seq(a,b) => (a,b)}
  //////////////////////////////////////////////////////

  final implicit class Enriched_slidingN_Iterator[A](val self: Iterator[A]) extends AnyVal {
    def sliding2(): Iterator[(A, A)] = self.sliding(2).map(_.toTuple2)
    def sliding3(): Iterator[(A, A, A)] = self.sliding(3).map(_.toTuple3)
    def sliding4(): Iterator[(A, A, A, A)] = self.sliding(4).map(_.toTuple4)
  }

  final implicit class Enriched_slidingN_GenTraversableLike[A, Repr <: GenTraversable[A]](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    def sliding2(): Iterator[(A, A)] = self.toIterator.sliding2()
    def sliding3(): Iterator[(A, A, A)] = self.toIterator.sliding3()
    def sliding4(): Iterator[(A, A, A, A)] = self.toIterator.sliding4()
  }

  //////////////////////////////////////////////////////
  // slyce
  //////////////////////////////////////////////////////

  final implicit class Enriched_slyce_GenTraversable[A, Repr <: GenTraversable[A]](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    def slyce[That](from: Int, until: Int)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      val start = if (from >= 0) from else self.size + from
      val end = if (until >= 0) until else self.size + until
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(end - start)
      b ++= self.slice(start, end).toIterator
      b.result
    }

    def slyce[That](range: Range)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      val start = if (range.start >= 0) range.start else self.size + range.start
      val end = (if (range.end >= 0) range.end else self.size + range.end) + (if (range.isInclusive) 1 else 0)
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(end - start)
      b ++= self.slice(start, end).toIterator
      b.result
    }
  }

  final implicit class Enriched_slyce_Iterator[A](val self: Iterator[A]) extends AnyVal {
    def slyce(from: Int, until: Int): Iterator[A] = {
      val start = if (from >= 0) from else throw new IllegalArgumentException("cannot slice Iterator with negative indices")
      val end = if (until >= 0) until else throw new IllegalArgumentException("cannot slice Iterator with negative indices")
      self.slice(start, end)
    }

    def slyce(range: Range): Iterator[A] = {
      val start = if (range.start >= 0) range.start else throw new IllegalArgumentException("cannot slice Iterator with negative indices")
      val end = (if (range.end >= 0) range.end else throw new IllegalArgumentException("cannot slice Iterator with negative indices")) + (if (range.isInclusive) 1 else 0)
      self.slice(start, end)
    }
  }

  final implicit class Enriched_slyce_String(val self: String) extends AnyVal {
    def slyce(from: Int, until: Int): String = {
      val start = if (from >= 0) from else self.size + from
      val end = if (until >= 0) until else self.size + until
      self.slice(start, end)
    }

    def slyce(range: Range): String = {
      val start = if (range.start >= 0) range.start else self.size + range.start
      val end = (if (range.end >= 0) range.end else self.size + range.end) + (if (range.isInclusive) 1 else 0)
      self.slice(start, end)
    }
  }

  //////////////////////////////////////////////////////
  // countCompare(p: A => Boolean, count: Int): Int
  //   - Compares the number of items satisfying a predicate to a test value.
  //   - Functionally equivalent to (but more efficient than):
  //         this.count(p).compareTo(count)
  //////////////////////////////////////////////////////

  final implicit class Enriched_countCompare_GenTraversableOnce[A](val self: GenTraversableOnce[A]) extends AnyVal {
    /**
     * Compares the number of items satisfying a predicate to a test value.
     *
     *   @param p       the predicate used to test elements.
     *   @param count   the test value that gets compared with the count.
     *   @return Int value `x` where
     *   {{{
     *        x <  0       if this.count(p) <  count
     *        x == 0       if this.count(p) == count
     *        x >  0       if this.count(p) >  count
     *   }}}
     *  The method as implemented here does not call `length` directly; its running time
     *  is `O(length min count)` instead of `O(length)`.
     */
    def countCompare(p: A => Boolean, count: Int): Int = {
      val itr = self.toIterator
      var i = 0
      while (itr.hasNext && i <= count) {
        if (p(itr.next))
          i += 1
      }
      i - count
    }
  }

  //////////////////////////////////////////////////////
  // takeWhileAg
  //////////////////////////////////////////////////////

  final implicit class Enriched_takeWhileAg_Iterator[A](val self: Iterator[A]) {
    def takeWhileAg(p: Iterable[A] => Boolean): Iterator[A] = {
      if (self.isEmpty) {
        self
      }
      else {
        new Iterator[A] {
          private[this] var nextElement: A = self.next
          private[this] val z = ListBuffer[A](nextElement)
          private[this] var done = false

          override def hasNext = !done && p(z)

          override def next = {
            if (hasNext) {
              val x = nextElement
              if (self.hasNext) {
                nextElement = self.next
                z += nextElement
              }
              else
                done = true
              x
            }
            else
              throw new NoSuchElementException("next on empty iterator")
          }
        }
      }
    }
  }

  final implicit class Enriched_takeWhileAg_GenTraversableLike[A, Repr <: GenTraversable[A]](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    def takeWhileAg[That](p: Iterable[A] => Boolean)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      bf(self.asInstanceOf[Repr]) ++= self.toIterator.takeWhileAg(p) result
    }
  }

  //////////////////////////////////////////////////////
  // takeSub[GenIterable[B]](n: Int): Repr[GenIterable[B]]
  //   - Take iterables from this collection until the total number of 
  //     elements in the taken items is about to exceed `n`.  The total number
  //     of elements will be less than or equal to `n`.
  //////////////////////////////////////////////////////

  final implicit class Enriched_takeSub_Iterator[A, R <: GenTraversable[A]](val self: Iterator[GenTraversableLike[A, R]]) {
    /**
     * Take iterables from this collection until the total number of elements
     * in the taken items is about to exceed `n`.  The total number of
     * elements will be less than or equal to `n`.
     *
     * @param n the maximum number of sub-elements to take
     * @return the new collection
     */
    def takeSub(n: Int): Iterator[R] = {
      require(n >= 0, "`n` cannot be negative")
      if (self.isEmpty) {
        self.asInstanceOf[Iterator[R]]
      }
      else {
        new Iterator[R] {
          private[this] var nextElement: R = self.next.asInstanceOf[R]
          private[this] var total: Int = nextElement.size

          override def hasNext = 0 <= total && total <= n

          override def next = {
            if (hasNext) {
              val x = nextElement
              if (self.hasNext) {
                nextElement = self.next.asInstanceOf[R]
                total += nextElement.size
              }
              else
                total = n + 1 // indicate to `hasNext` that there should be no more elements
              x
            }
            else
              throw new NoSuchElementException("next on empty iterator")
          }
        }
      }
    }
  }

  final implicit class Enriched_takeSub_GenTraversableLike[A, R <: GenTraversable[A], Repr <: GenTraversable[GenTraversable[A]]](val self: GenTraversableLike[GenTraversableLike[A, R], Repr]) extends AnyVal {
    /**
     * Take iterables from this collection until the total number of elements
     * in the taken items is about to exceed `n`.  The total number of
     * elements will be less than or equal to `n`.
     *
     * @param n the maximum number of sub-elements to take
     * @return the new collection
     */
    def takeSub[That](n: Int)(implicit bf: CanBuildFrom[Repr, R, That]): That = {
      bf(self.asInstanceOf[Repr]) ++= self.toIterator.takeSub(n) result
    }
  }

  //////////////////////////////////////////////////////
  // asc/desc
  //////////////////////////////////////////////////////

  final implicit class Enriched_AscDesc_GenTraversableOnce[K, V](val self: GenTraversableOnce[(K, V)])(implicit ord: scala.math.Ordering[V]) /*extends AnyVal */ {
    def asc: Vector[(K, V)] = self.toVector.sortBy(t => t._2)
    def desc: Vector[(K, V)] = self.toVector.sorted((ord on ((t: (K, V)) => t._2)).reverse)
  }

  //////////////////////////////////////////////////////
  // Iterator.last
  // Iterator.takeRight
  // Iterator.dropRight
  //////////////////////////////////////////////////////

  final implicit class Enriched_last_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * @return The last item in the iterator.  Note that the iterator will be consumed after calling.
     */
    def last(): A = {
      if (!self.hasNext) throw new AssertionError("cannot call Iterator.last on an empty iterator")
      var a = self.next()
      while (self.hasNext) a = self.next()
      a
    }

    /**
     * @return The last n items of the iterator.  Note that the iterator will be consumed after calling unless n <= 0.
     */
    def takeRight(n: Int): Vector[A] = {
      if (self.hasNext && n > 0) self.sliding(n).last.toVector
      else Vector.empty
    }

    /**
     * @return All but the last n items of the iterator.  Note that the iterator will be consumed after calling.
     */
    def dropRight(n: Int): Vector[A] = self.toVector.dropRight(n)
  }

  //////////////////////////////////////////////////////
  // GenTraversableOnce.only
  //////////////////////////////////////////////////////

  final implicit class Enriched_only_GenTraversableOnce[A](val self: GenTraversableOnce[A]) extends AnyVal {
    /**
     * Return the only element in the collection, or error if there is not exactly one element.
     *
     * @return the only element
     */
    def only(): A = {
      val itr = self.toIterator
      assert(itr.hasNext, "cannot call `only` on empty collection.")
      val a = itr.next
      assert(!itr.hasNext, f"cannot call `only` on collection with ${itr.size + 1} elements.")
      a
    }
  }

  final implicit class Enriched_only_Array[A](val self: Array[A]) extends AnyVal {
    /**
     * Return the only element in the collection, or error if there is not exactly one element.
     *
     * @return the only element
     */
    def only(): A = {
      assert(self.size == 1, f"cannot call `only` on collection with ${self.size + 1} elements.")
      self(0)
    }
  }

  //////////////////////////////////////////////////////
  // No-Op
  //////////////////////////////////////////////////////
  final implicit class Enriched_noop_GenTraversableLike[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    def noop[That](f: A => _)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      self.map { x => f(x); x }
    }
  }

  private[this] class NoOpIterator[A](self: Iterator[A], f: A => _) extends Iterator[A] {
    def hasNext = self.hasNext
    def next() = {
      val x = self.next
      f(x)
      x
    }
  }
  final implicit class Enriched_noop_Iterator[A](val self: Iterator[A]) extends AnyVal {
    def noop(f: A => _): Iterator[A] = new NoOpIterator(self, f)
  }

  //////////////////////////////////////////////////////
  // shuffle
  //////////////////////////////////////////////////////

  final implicit class Enriched_shuffle_Seq[A, Repr](val self: SeqLike[A, Repr]) extends AnyVal {
    def shuffle[That](implicit bf: CanBuildFrom[Repr, A, That]): That =
      (bf(self.asInstanceOf[Repr]) ++= Random.shuffle(self)).result
  }

  final implicit class Enriched_shuffle_Iterator[A](val self: Iterator[A]) extends AnyVal {
    def shuffle: Iterator[A] = Random.shuffle(self)
  }

  //////////////////////////////////////////////////////
  // toBitSet
  //////////////////////////////////////////////////////

  final implicit class Enriched_toBitSet_GenTraversableOnce(val self: GenTraversableOnce[Int]) extends AnyVal {
    def toBitSet: BitSet = BitSet() ++ self
  }

  //////////////////////////////////////////////////////
  // mutable.Map.updateWith, mutable.Map.updateOrElseWith
  //////////////////////////////////////////////////////

  final implicit class Enriched_updateWith_MutableMap[K, V](val self: mutable.Map[K, V]) extends AnyVal {
    def updateWith(key: K)(f: (V => V)): mutable.Map[K, V] = {
      self(key) = f(self(key))
      self
    }

    def updateOrElseWith(key: K, default: V)(f: (V => V)): mutable.Map[K, V] = {
      self(key) = f(self.getOrElse(key, default))
      self
    }
  }

  //////////////////////////////////////////////////////
  // PARALLEL / SEQUENTIAL
  //   - obnoxious versions of the .par and .seq methods 
  //////////////////////////////////////////////////////

  final implicit class Enriched_PARALLEL_Parallelizable[+A, +ParRepr <: Parallel](val self: Parallelizable[A, ParRepr]) extends AnyVal {
    def PARALLEL = self.par
  }
  final implicit class Enriched_SEQUENTIAL_Iterator[+A](val self: Iterator[A]) extends AnyVal {
    def SEQUENTIAL = self.seq
  }
  final implicit class Enriched_SEQUENTIAL_GenTraversableLike[+A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    def SEQUENTIAL[That](implicit bf: CanBuildFrom[Repr, A, That]): That = (bf(self.asInstanceOf[Repr]) ++= self.seq).result
  }

  //////////////////////////////////
  // CollectionUtil.scala
  //////////////////////////////////

  //////////////////////////////////
  // CommandLineUtil.scala: Command Line
  //////////////////////////////////

  private[this] val OptionRegex = "--(.*)".r

  def parseArgs(args: Array[String]) = {
    val parsedArgs =
      ("" +: args.toVector) // prepend an empty arg so sliding will work
        .sliding(2).flatMap {
          case Seq(OptionRegex(option), argument) => Some(option, argument) // if the first thing is an option
          case Seq(_, OptionRegex(_)) => None // if the second thing is an option
          case Seq(_, argument) => Some("argument", argument) // if no options are involved, then it's a normal argument
          case Seq(_) => None
        }

    val (argumentList, optionList) =
      parsedArgs.partition { // separate normal arguments from options
        case ("argument", argument) => true
        case _ => false
      }

    val arguments = argumentList.map(_._2).toVector // arguments are a Vector
    val options = optionList.groupByKey.map { case (opt, Coll(v)) => opt -> v; case (opt, _) => sys.error(f"option --${opt} given twice") } // options are a Map
    (arguments, CommandLineOptions(options))
  }

  case class CommandLineOptions(options: Map[String, String]) {
    private[this] val retrieved = collection.mutable.Set.empty[String]

    def get(key: String) = { retrieved += key; options.get(key) }
    def apply(key: String) = get(key).getOrElse(throw new NoSuchElementException(f"--$key not specified : ${options}"))
    def s(key: String) = apply(key)
    def s(key: String, default: String) = get(key).getOrElse(default)
    def i(key: String) = apply(key).toInt
    def i(key: String, default: Int) = get(key).fold(default)(_.toInt)
    def l(key: String) = apply(key).toLong
    def l(key: String, default: Long) = get(key).fold(default)(_.toLong)
    def d(key: String) = apply(key).toDouble
    def d(key: String, default: Double) = get(key).fold(default)(_.toDouble)
    def b(key: String) = apply(key).toBoolean
    def b(key: String, default: Boolean) = get(key).fold(default)(_.toBoolean)
    def contains(key: String) = options.get(key).isDefined

    def unusedOptions = options.keySet -- retrieved

    def toVector = options.toVector
    def toMap = options
  }

  //////////////////////////////////
  // FileUtil.scala
  //////////////////////////////////

  type File = java.io.File

  object File {
    def apply(parent: File, child: String) = new File(parent, child)
    def apply(path: String) = new File(path)
    def apply(path: String*) = new File(pathjoin(path))
    def apply(uri: URI) = new File(uri)
  }

  def file(parent: File, child: String) = new File(parent, child)
  def file(path: String) = new File(path)
  def file(path: String*) = new File(pathjoin(path))
  def file(uri: URI) = new File(uri)

  def pathjoin(part: String, parts: String*): String = {
    pathjoin(part +: parts)
  }

  def pathjoin(parts: Seq[String]): String = {
    val start = if (parts.head.startsWith(separator)) separator else ""
    parts.flatMap(_.split(separator)).filter(_.nonEmpty).mkString(start, separator, "")
  }

  /**
   * Generate a temporary filename but do not actually create the file in the
   * filesystem.
   */
  def mktemp(prefix: String = "temp-", suffix: String = ""): File = {
    val f = createTempFile(prefix, suffix)
    f.delete()
    f
  }

  implicit class EnhancedFile(val self: File) extends AnyVal {

    def path = {
      self.getPath
    }

    def name = {
      self.getName
    }

    def parent = {
      Option(self.getParentFile)
    }

    /**
     * Separate the filename from the parent directory.
     * Return Some(parentDir, filename) if there is a parent directory,
     * and None otherwise.
     */
    def parentFilename = {
      (self.parent, self.name)
    }

    /**
     * Get the path as a sequence of strings.
     */
    def pathSeq: Vector[String] = {
      val (parent, file) = parentFilename
      parent.map(_.pathSeq).getOrElse(Vector()) :+ file
    }

    /**
     * Creating the file's containing directory structure if necessary.
     */
    def mkParentDir() {
      self.parent.foreach(_.mkdirs())
    }

    def ls(): Vector[File] = {
      assert(self.exists, s"'$self' does not exist")
      assert(self.isDirectory, s"'$self' is not a directory")
      self.listFiles.toVector
    }

    def ls(regex: Regex, pathMatch: Boolean = false): Vector[File] = {
      assert(self.exists, s"'$self' does not exist")
      assert(self.isDirectory, s"'$self' is not a directory")
      val files = self.listFiles.toVector
      def getName(f: File) =
        if (pathMatch) f.getAbsolutePath
        else f.getName
      files.filter(f => regex.pattern.matcher(getName(f)).matches)
    }

    /**
     * List all files (but not directories), searching recursively through sub-directories.
     */
    def listFilesRecursive(): Vector[File] = {
      assert(self.exists, s"'$self' does not exist")
      assert(self.isDirectory, s"'$self' is not a directory")
      self.ls.flatMap { f =>
        if (f.isDirectory)
          f.listFilesRecursive
        else
          Vector(f)
      }
    }

    /**
     * List all files (but not directories), searching recursively through sub-directories.
     */
    def listFilesRecursive(regex: Regex, pathMatch: Boolean = false): Vector[File] = {
      assert(self.exists, s"'$self' does not exist")
      assert(self.isDirectory, s"'$self' is not a directory")
      self.ls(regex, pathMatch) ++ self.ls.flatMap { f =>
        if (f.isDirectory)
          f.listFilesRecursive(regex, pathMatch)
        else
          Vector()
      }
    }

    /**
     * Return a path to this relative to the given directory.
     */
    def relativeTo(dir: File): Option[File] = {
      val dirPath = dir.getAbsolutePath
      val selfPath = self.getAbsolutePath
      if (selfPath.startsWith(dirPath)) {
        val a = selfPath.drop(dirPath.length)
        val s = if (a.startsWith(separator)) a.drop(1) else a
        Some(File(s))
      }
      else
        None
    }

    /**
     * Read the contents of this file, making sure to close the file after all
     * lines have been read.
     */
    def readLines: Iterator[String] = {
      readLines("UTF-8")
    }

    /**
     * Read the contents of this file, making sure to close the file after all
     * lines have been read.
     */
    def readLines(encoding: String): Iterator[String] = {
      SelfClosingBufferedReaderIterator(bufferedReader(self, encoding))
    }

  }

  def bufferedReader(file: File, encoding: String = "UTF-8") = {
    new BufferedReader(new InputStreamReader(new FileInputStream(file), encoding))
  }

  /**
   * Get an Iterator over the lines in the BufferedReader.
   */
  case class BufferedReaderIterator(reader: BufferedReader) extends Iterator[String] {
    override def hasNext() = reader.ready
    override def next() = reader.readLine()
  }

  /**
   * Get a BufferedReader for GZIP files
   */
  object GzFileBufferedReader {
    def apply(file: File, encoding: String = "UTF-8"): BufferedReader = {
      new BufferedReader(
        new InputStreamReader(
          new GZIPInputStream(
            new FileInputStream(file)), encoding))
    }
  }

  /**
   * Iterator over the lines in the BufferedReader.  The reader will
   * automatically close itself when the end is reached.  This gets around the
   * problem of having to all of your processing inside the `using` block.
   */
  case class SelfClosingBufferedReaderIterator(bufferedReader: BufferedReader) extends Iterator[String] {
    private[this] val blockItr = BufferedReaderIterator(bufferedReader)
    private[this] var finished = false
    override def next() = {
      hasNext()
      if (finished) throw new NoSuchElementException("next on empty iterator")
      val n = blockItr.next
      hasNext()
      n
    }
    override def hasNext() = {
      if (finished)
        false
      else {
        val hn = blockItr.hasNext
        if (!hn) {
          finished = true
          bufferedReader.close()
        }
        hn
      }
    }
  }

  def bufferedWriter(file: File, encoding: String = "UTF-8") = {
    new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), encoding))
  }

  def findBinary(name: String, binDir: Option[String] = None, envar: Option[String] = None): String = {
    val checked = collection.mutable.Buffer[String]()

    for (d <- binDir) {
      val path = pathjoin(d, name)
      if (File(path).exists)
        return path
      else
        checked += path
    }

    for (ev <- envar; envpath <- Option(System.getenv(ev))) {
      val path = envpath + "/" + name
      if (File(path).exists)
        return path
      else
        checked += path
    }

    try {
      val found = scala.sys.process.Process(List("which", name)).!!
      return found.trim
    }
    catch {
      case _: Throwable => checked += s"which $name"
    }

    throw new RuntimeException("No binary found.  Checked the following:\n" + checked.map((" ") * 16 + _).mkString("\n"))
  }

  /**
   * Open a file for reading, execute a block of code, and ensure that the
   * file is closed when finished.
   */
  def readUsing[R](file: File, encoding: String = "UTF-8")(block: BufferedSource => R): R = {
    using(Source.fromFile(file, encoding))(block)
  }

  /**
   * Open a file for writing (creating the containing directory structure if
   * necessary), execute a block of code, and ensure that the file is closed
   * when finished.
   */
  def writeUsing[R](file: File, encoding: String = "UTF-8")(block: BufferedWriter => R): R = {
    file.mkParentDir()
    using(bufferedWriter(file, encoding))(block)
  }

  /**
   * Add a method `writeLine` to Writer classes
   */
  implicit class WriterWithWriteLine(val self: Writer) extends AnyVal {
    def writeLine(line: Any) { self.write(line + "\n") }
    def writeLine() { self.write("\n") }

    def wl(line: Any) = writeLine(line)
    def wl() = writeLine()
  }

  //////////////////////////////////
  // NumberUtil.scala
  //////////////////////////////////

  implicit class Enriched_Int(val self: Int) extends AnyVal {
    /**
     * Shorthand for a range from this Int to the max integer value.
     */
    def up: Range = self to (if (self > 0) Int.MaxValue else (Int.MaxValue + (self - 1)))
    def upi: Iterator[Int] = Iterator.from(self)

    /**
     * Shorthand for a range from this to n by -1
     */
    def downto(n: Int): Range = self to n by -1

    def pow(e: Int): Int = (1 until e).foldLeft(self)((z, _) => z * self)
    def **(e: Int): Int = pow(e)
    def pow(e: Double): Double = scala.math.pow(self, e)
    def **(e: Double): Double = scala.math.pow(self, e)

    def toLogDouble: LogDouble = LogDouble(self)
    def log: LogDouble = toLogDouble
  }

  implicit class Enriched_Double(val self: Double) extends AnyVal {
    def pow(e: Double): Double = scala.math.pow(self, e)
    def **(e: Double): Double = scala.math.pow(self, e)

    def toLogDouble: LogDouble = LogDouble(self)
    def log: LogDouble = toLogDouble
  }

  /**
   * A mutable number-holding object
   */
  class MutableNumber[N](private[this] var i: N)(implicit num: Numeric[N]) {
    def this()(implicit num: Numeric[N]) = this(num.zero)
    def +=(o: N) = { i = num.plus(i, o); this }
    def get = i
  }

  final implicit class Enriched_dot_GenTraversableOnce[A](val self: GenTraversableOnce[A])(implicit num: Numeric[A]) {
    /**
     * Find the dot product of the two vectors.
     *
     * @return the dot product
     * @throws RuntimeException thrown if collections differ in length
     */
    def dot(other: GenTraversableOnce[A]): A = {
      (self.toIterator zipSafe other).foldLeft(num.zero) { case (z, (a, b)) => num.plus(z, num.times(a, b)) }
    }
  }

  //////////////////////////////////
  // Pattern.scala
  //////////////////////////////////

  /**
   * Make it possible to do:
   *   val UInt(x) = "-15"
   */
  object UInt {
    val IntRE = """^(-?\d+)$""".r
    def unapply(v: String): Option[Int] = v match {
      case IntRE(s) => Some(s.toInt)
      case _ => None
    }
  }

  /**
   * Make it possible to do:
   *   val UDouble(x) = "-15.0"
   */
  object UDouble {
    val DoubleRE = """^(-?\d+\.?\d*|-?\d*\.?\d+)$""".r
    def unapply(v: String): Option[Double] = v match {
      case DoubleRE(s) => Some(s.toDouble)
      case _ => None
    }
  }

  /**
   * Make it possible to do:
   *   val UBoolean(x) = "true"
   */
  object UBoolean {
    val booleanRE = """([Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee])""".r
    def unapply(v: String): Option[Boolean] = v match {
      case booleanRE(s) => Some(s.toBoolean)
      case _ => None
    }
  }

  /**
   * Make it possible to do:
   *   val a -> b = (1,2)
   */
  object -> {
    def unapply[A, B](pair: (A, B)): Option[(A, B)] = Some(pair)
  }

  /**
   * Make it possible to do:
   *   val Coll(a, b, c @ _*) = Set(1,2,3,4,5)
   *   val Coll(d -> e) = Map(6 -> 'g)
   */
  object Coll {
    def unapplySeq[T](s: Iterable[T]): Option[Vector[T]] = Some(s.toVector)
  }

  /**
   * Make it possible to do:
   *   val SetHeadTail(a, bs) = Set(1,2,3)
   * where `a` is an element and `bs` is the set of remaining elements
   */
  object SetHeadTail {
    def unapply[T](s: Set[T]): Option[(T, Set[T])] = {
      if (s.isEmpty) None
      else {
        val a +: bs = s.toVector
        Some((a, bs.toSet))
      }
    }
  }

  /**
   * Make it possible to interpret and create range strings:
   *
   *   val aVector = RangeString("1-3, 5, 6, 7-9,11-12, 13-14")
   *   val RangeString(s) = "1-3, 5, 6, 7-9,11-12, 13-14"  // makes the same Vector
   *
   *   val rangeString = RangeString(Seq(1,2,3,5,7,8,9))   // rangeString = "1-3,5,7-9"
   *   val RangeString(rangeString) = Seq(1,2,3,5,7,8,9)   // makes the same String
   */
  object RangeString {
    val RangeRE = """^(\d+)-(\d+)$""".r
    val OpenRangeRE = """^(\d+)-$""".r

    /**
     * Interpret the range string as a sequence of integers
     */
    def apply(s: String): Vector[Int] = {
      s.replaceAll("\\s+", "").split(",").flatMap {
        case UInt(i) => i to i
        case RangeRE(UInt(b), UInt(e)) if b <= e => b to e
      }.toVector
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

    def unapply(s: String): Option[Vector[Int]] = Some(apply(s))
    def unapply(seq: Seq[Int]): Option[String] = Some(apply(seq))
  }

  class RangeString(max: Int) {
    /**
     * Interpret the range string as a sequence of integers
     */
    def apply(s: String): Vector[Int] = {
      s.replaceAll("\\s+", "").split(",").flatMap {
        case UInt(i) => i to i
        case RangeString.RangeRE(UInt(b), UInt(e)) if b <= e => b to e
        case RangeString.OpenRangeRE(UInt(b)) => b to max
      }.toVector
    }
    def unapply(s: String): Option[Vector[Int]] = Some(apply(s))
  }

  //////////////////////////////////
  // StringUtil.scala
  //////////////////////////////////

  val WhitespaceRe = """\s*""".r
  val RTrimRe = """(.*\S)\s*""".r

  implicit class EnrichedString(val self: String) extends AnyVal {

    /**
     * Trim whitespace only from the right side of the string
     */
    def rtrim = self match {
      case WhitespaceRe() => ""
      case RTrimRe(trimmed) => trimmed
    }

    /**
     * Split on newlines
     */
    def splitlines: Vector[String] = self.lsplit("\n")

    /**
     * Split on whitespace
     */
    def splitWhitespace: Vector[String] = self.lsplit("\\s+")

    /**
     * Split a string into `limit` pieces, starting from the left side.
     */
    def lsplit(str: String, keepDelimiter: KeepDelimiter = DropDelimiter): Vector[String] = {
      new RegexMatcherSplitIterator(self, str, keepDelimiter).toVector
        .dropRightWhile { case (b, e) => b == e && b > 0 }
        .map { case (b, e) => self.substring(b, e) }
    }

    /**
     * Split a string into `limit` pieces, starting from the left side.
     */
    def lsplit(str: String, limit: Int): Vector[String] = {
      lsplit(str, limit, DropDelimiter)
    }

    /**
     * Split a string into `limit` pieces, starting from the left side.
     */
    def lsplit(str: String, limit: Int, keepDelimiter: KeepDelimiter): Vector[String] = {
      val allSpans = new RegexMatcherSplitIterator(self, str, keepDelimiter).take(limit).toVector
      val leftSpans :+ h = allSpans
      val spans = leftSpans :+ (h._1 -> self.length())
      spans.map { case (b, e) => self.substring(b, e) }
    }

    /**
     * Split a string into `limit` pieces, starting from the right side.
     */
    def rsplit(str: String, keepDelimiter: KeepDelimiter = DropDelimiter): Vector[String] = {
      new RegexMatcherSplitIterator(self, str, keepDelimiter).toVector
        .dropWhile { case (b, e) => b == e }
        .map { case (b, e) => self.substring(b, e) }
    }

    /**
     * Split a string into `limit` pieces, starting from the right side.
     */
    def rsplit(str: String, limit: Int): Vector[String] = {
      rsplit(str, limit, DropDelimiter)
    }

    /**
     * Split a string into `limit` pieces, starting from the right side.
     */
    def rsplit(str: String, limit: Int, keepDelimiter: KeepDelimiter): Vector[String] = {
      val allSpans = new RegexMatcherSplitIterator(self, str, keepDelimiter).toVector
      val spans =
        if (allSpans.size > limit) {
          val h +: rightSpans = allSpans.takeRight(limit)
          (0 -> h._2) +: rightSpans
        }
        else
          allSpans
      spans.map { case (b, e) => self.substring(b, e) }
    }

    def groups(regex: Regex) = regex.groups(self)
    def groupsOption(regex: Regex) = regex.groupsOption(self)
    def firstGroup(regex: Regex) = regex.firstGroup(self)
    def allGroups(regex: Regex) = regex.allGroups(self)

    def padLeft(to: Int, padding: String = " ") = {
      val toadd = to - self.length
      if (toadd > 0) ((padding * toadd) + self).takeRight(to) else self
    }

    def padRight(to: Int, padding: String = " ") = {
      val toadd = to - self.length
      if (toadd > 0) (self + (padding * toadd)).take(to) else self
    }

    /**
     * Add newlines to a string such that each line is `width` or less.  Line
     * splits happen only on whitespace.  In the case where a single 'word'
     * is longer than `width`, the newline will simply be inserted after the
     * word.
     */
    def wrapToLines(width: Int = 80): Vector[String] = {
      self.split("\n").toVector.flatMap { line =>
        val (completeLines, lastLine) =
          line.split("\\s+").foldLeft((Vector[String](), "")) {
            case ((lines, currLine), tok) =>
              if (currLine.size + tok.size + 1 > width)
                (lines :+ currLine, tok)
              else if (currLine.isEmpty)
                (lines, tok)
              else
                (lines, currLine + " " + tok)
          }
        completeLines :+ lastLine
      }
      //lines.map(s => f"$s%-80s|").mkString("\n")
    }

    /**
     * Add newlines to a string such that each line is `width` or less.  Line
     * splits happen only on whitespace.  In the case where a single 'word'
     * is longer than `width`, the newline will simply be inserted after the
     * word.
     */
    def wrap(width: Int = 80): String = {
      this.wrapToLines(width).mkString("\n")
    }

    /**
     * Indent the left side of each line by the given number of spaces.
     */
    def indent(spaces: Int): String = {
      indent(" " * spaces)
    }

    /**
     * Append the given string to the left side of each line.
     */
    def indent(leftColumn: String): String = {
      self.split("\n").map(leftColumn + _).mkString("\n")
    }

  }

  def sideBySideStrings(spaceBuffer: Int, columns: String*) = {
    sideBySide(spaceBuffer, columns.map(_.split("\n").toVector): _*).mkString("\n")
  }

  def sideBySide(spaceBuffer: Int, columns: Vector[String]*) = {
    val maxHeight = columns.map(_.size).max
    val vertBuffered =
      for (c <- columns) yield {
        c ++ Vector.fill(maxHeight - c.size)("")
      }
    val horizBuffered =
      (for (c <- vertBuffered.dropRight(1)) yield {
        val maxLineLength = c.map(_.length).max
        for (line <- c) yield {
          line + (" " * (maxLineLength - line.length))
        }
      }) :+ vertBuffered.last
    for (columnLines <- horizBuffered.transpose) yield {
      columnLines.mkString(" " * spaceBuffer)
    }
  }

  private class RegexMatcherSplitIterator(str: String, pattern: String, keepDelimiter: KeepDelimiter = DropDelimiter) extends Iterator[(Int, Int)] {
    val m = pattern.r.pattern.matcher(str)
    var prevE: Int = 0
    var queued: Option[(Int, Int)] = None
    var nextE: Option[Int] = Some(0)

    def hasNext() =
      if (queued.isDefined) {
        true
      }
      else if (m.find()) {
        queued =
          if (keepDelimiter == KeepDelimiterAsLast)
            Some(prevE -> m.end)
          else
            Some(prevE -> m.start)
        nextE =
          if (keepDelimiter == KeepDelimiterAsFirst)
            Some(m.start)
          else
            Some(m.end)
        true
      }
      else if (nextE.isDefined) {
        queued = Some(nextE.get -> str.length())
        nextE = None
        true
      }
      else {
        false
      }

    def next() =
      if (hasNext) {
        val n = queued.get
        prevE = nextE.getOrElse(-1)
        queued = None
        n
      }
      else
        Iterator().next()

    override def toString = s"RegexMatcherSplitIterator(string=$str, pattern=$pattern, keepDelimiter=$keepDelimiter, prevE=$prevE, queued=$queued, nextE=$nextE, hasNext=$hasNext)"
  }

  implicit class EnrichedRegex(val self: Regex) extends AnyVal {
    def matches(s: String): Boolean = self.pattern.matcher(s).matches
    def apply(s: String) = groups(s)
    def groups(s: String) = groupsOption(s).getOrElse(sys.error(self.pattern.matcher(s).group))
    def groupsOption(s: String) = self.unapplySeq(s)
    def firstGroup(s: String) = self.findFirstMatchIn(s).map(_.subgroups)
    def allGroups(s: String) = self.findAllMatchIn(s).map(_.subgroups)
  }

  //////////////////////////////////
  // Time.scala
  //////////////////////////////////

  def time[T](name: String, block: => T): T = {
    time(name, block, println)
  }

  def time[T, R](name: String, block: => T, log: String => R): T = {
    log(s"starting: $name")
    val (r, t) = timer(block)
    log(s"finished: $name in $t seconds")
    r
  }

  def time1[T](name: String, block: => T): T = {
    time1(name, block, println)
  }

  def time1[T, R](name: String, block: => T, log: String => R): T = {
    val (r, t) = timer(block)
    log(s"$name - $t seconds")
    r
  }

  def timer[T](block: => T): (T, Double) = {
    val startTime = System.currentTimeMillis()
    val r = block
    (r, (System.currentTimeMillis() - startTime) / 1000.0)
  }

  //////////////////////////////////
  // Subprocess.scala
  //////////////////////////////////

  /**
   * A class for conveniently running command-line operations.
   *
   * e.g.
   *   val cmd = Subprocess.findBinary("tr")
   *   cmd.args("l", "L").call("hello")   // "heLLo"
   *
   *   val ls = Subprocess("ls")
   *   ls(".").call()
   *
   * @author Dan Garrette (dhgarrette@gmail.com)
   */
  class Subprocess(binary: String, args: Seq[String]) {

    /**
     * Create a callable subprocess object
     *
     * @param args    A list of command-line arguments.
     * @return new Subprocess
     */
    def args(args: String*) = new Subprocess(binary, args)

    /**
     * Create a callable subprocess object
     *
     * @param newArgs   A list of command-line arguments to be appended to the end of the existing arg list.
     * @return new Subprocess
     */
    def appendArgs(newArgs: String*) = new Subprocess(binary, args ++ newArgs)

    /**
     * Alias for `appendArgs`
     */
    def apply(newArgs: String*) = new Subprocess(binary, args ++ newArgs)

    def base() = new Subprocess(binary, Nil)
    def noargs() = this.base

    /**
     * Call the binary
     *
     * @return stdout
     */
    def call(): String = {
      val (exitcode, stdout, stderr) = callAllReturns()
      if (exitcode != 0)
        sys.error(s"ERROR CALLING: $binary ${args.mkString(" ")}\nReturncode: $exitcode\n$stderr")
      stdout
    }

    /**
     * Call the binary with the given input
     *
     * @param input A string whose contents are used as stdin
     * @return stdout
     */
    def call(inputStr: String): String = {
      val (exitcode, stdout, stderr) = callAllReturns(inputStr)
      if (exitcode != 0)
        sys.error(s"ERROR CALLING: $binary ${args.mkString(" ")}\nReturncode: $exitcode\n$stderr")
      stdout
    }

    /**
     * Call the binary
     *
     * @return (returncode, stdout, stderr)
     */
    def callAllReturns(): (Int, String, String) = {
      val out = new StringBuilder
      val err = new StringBuilder
      val exitcode = callWithStreams(out, err)
      (exitcode, out.result, err.result)
    }

    /**
     * Call the binary with the given input
     *
     * @param input A string whose contents are used as stdin
     * @return (returncode, stdout, stderr)
     */
    def callAllReturns(input: String): (Int, String, String) = {
      val out = new StringBuilder
      val err = new StringBuilder
      val exitcode = callWithStreams(input, out, err)
      (exitcode, out.result, err.result)
    }

    /**
     * Call the binary and give objects to which the outputs will be streamed.
     *
     * @param out   an appendable object where stdout information is written
     * @param err   an appendable object where stderr information is written
     * @return exitcode
     */
    def callWithStreams[T, R](out: Appendable[T, String], err: Appendable[R, String]): Int = {
      Process(binary +: args) ! ProcessLogger(out.append(_).append("\n"), err.append(_).append("\n"))
    }

    /**
     * Call the binary with the given input and give objects to which the
     * outputs will be streamed.
     *
     * @param input A string whose contents are used as stdin
     * @param out   an appendable object where stdout information is written
     * @param err   an appendable object where stderr information is written
     * @return exitcode
     */
    def callWithStreams[T, R](input: String, out: Appendable[T, String], err: Appendable[R, String]): Int = {
      Process(List("echo", input)) #| Process(binary +: args) ! ProcessLogger(out.append(_).append("\n"), err.append(_).append("\n"))
    }
  }

  object Subprocess {

    def apply(binary: String, args: String*) = new Subprocess(binary, args)

    /**
     * Look up a binary and create a Subprocess object for it.
     */
    def findBinary(binaryName: String, binDir: Option[String] = None, envar: Option[String] = None): Subprocess =
      new Subprocess(dhg.util.findBinary(binaryName, binDir, envar), Nil)
  }

  /**
   * Trait for Appendable classes that can serve as `out` or `err` streams.
   */
  trait Appendable[T, A] {
    def self: T
    def append(a: A): this.type
  }

  implicit class AppendableStringBuilder(val self: StringBuilder) extends Appendable[StringBuilder, String] {
    def append(a: String) = { self.append(a); this }
  }

  implicit class AppendableGrowable[A, T <: Growable[A]](val self: T) extends Appendable[T, A] {
    def append(a: A) = { self += a; this }
  }

  implicit class AppendableWriter[T <: Writer](val self: T) extends Appendable[T, String] {
    def append(a: String) = { self.write(a); this }
  }

  implicit class AppendablePrintStream[A](val self: PrintStream) extends Appendable[PrintStream, A] {
    def append(a: A) = { self.print(a); this }
  }

  //////////////////////////////////
  // Other.scala: Other stuff
  //////////////////////////////////

  implicit class Enriched_Validation[E, A](val v: Validation[E, A]) extends AnyVal {
    def getOrElseThrow(): A = v match {
      case Success(a) => a
      case Failure(e) => throw new RuntimeException(e.toString)
    }
  }

  //////////////////////////////////
  // FastMathUtil.scala
  //////////////////////////////////

  object FastMathUtil {

    def sum(a: Array[Double], length: Int): Double = {
      assert(length <= a.length, s"Passed in a length of $length to sum, for an array of length ${a.length}")
      var accum = 0.0
      var i = 0
      while (i < length) {
        accum += a(i)
        i += 1
      }
      accum
    }

    def activeSum(a: Array[Double], active: Array[Int], activeCount: Int): Double = {
      assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to activeSum, for an active array of length ${active.length}")
      assert(activeCount <= a.length, s"Passed in an activeCount of $activeCount to activeSum, for an array of length ${a.length}")
      var accum = 0.0
      var i = 0
      while (i < activeCount) {
        accum += a(active(i))
        i += 1
      }
      accum
    }

    /**
     * Sums together things in log space.
     * @return log(exp(a) + exp(b))
     *
     * stolen from breeze
     */
    def logSum(a: Double, b: Double) = {
      if (a.isNegInfinity) b
      else if (b.isNegInfinity) a
      else if (a < b) b + scala.math.log1p(exp(a - b))
      else a + scala.math.log1p(exp(b - a))
    }

    /**
     * Sums together things in log space.
     * @return log(\sum exp(a_i))
     *
     * stolen from breeze
     */
    def logSum(a: Double, b: Double, c: Double*): Double = {
      logSum(logSum(a, b) +: c)
    }

    /**
     * Sums together things in log space.
     * @return log(\sum exp(a_i))
     *
     * stolen from breeze
     */
    def logSum(iter: Iterator[Double], max: Double): Double = {
      require(iter.hasNext)
      if (max.isInfinite) {
        max
      }
      else {
        val aux = (0.0 /: iter) {
          (acc, x) => if (x.isNegInfinity) acc else acc + exp(x - max)
        }
        if (aux != 0)
          max + scala.math.log(aux)
        else
          max
      }
    }

    /**
     * Sums together things in log space.
     * @return log(\sum exp(a_i))
     *
     * stolen from breeze
     */
    def logSum(a: Seq[Double]): Double = {
      a.length match {
        case 0 => Double.NegativeInfinity
        case 1 => a(0)
        case 2 => logSum(a(0), a(1))
        case _ => logSum(a.iterator, a reduceLeft (_ max _))
      }
    }

    /**
     * Sums together the first length elements in log space.
     * The length parameter is used to make things faster.
     *
     * This method needs to be fast. Don't scala-ify it.
     *
     * log(\sum^length exp(a_i))
     *
     * stolen from breeze
     */
    def logSum(a: Array[Double], length: Int): Double = {
      assert(length <= a.length, s"Passed in a length of $length to logSum, for an array of length ${a.length}")
      if (length == 0) Double.NegativeInfinity
      else if (length == 1) a(0)
      else if (length == 2) logSum(a(0), a(1))
      else {
        val m = max(a, length)
        if (m.isInfinite) m // avoids NaN issue
        else {
          var i = 0
          var accum = 0.0
          while (i < length) {
            accum += scala.math.exp(a(i) - m)
            i += 1
          }
          m + scala.math.log(accum)
        }
      }
    }

    def activeLogSum(a: Array[Double], active: Array[Int], activeCount: Int): Double = {
      assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to activeLogSum, for an active array of length ${active.length}")
      assert(activeCount <= a.length, s"Passed in an activeCount of $activeCount to activeLogSum, for an array of length ${a.length}")

      if (activeCount == 0) Double.NegativeInfinity
      else if (activeCount == 1) a(active(0))
      else if (activeCount == 2) logSum(a(active(0)), a(active(1)))
      else {
        val m = activeMax(a, active, activeCount)
        if (m.isInfinite) m // avoids NaN issue
        else {
          var i = 0
          var accum = 0.0
          while (i < activeCount) {
            accum += scala.math.exp(a(active(i)) - m)
            i += 1
          }
          m + scala.math.log(accum)
        }
      }
    }

    /**
     * fast versions of max. Useful for the fast logsum.
     *
     * stolen from breeze
     */
    def max(a: Array[Double], length: Int) = {
      assert(length != 0, s"Cannot compute max for a length of zero. (Array has length ${a.length})")
      assert(length <= a.length, s"Passed in a length of $length to max, for an array of length ${a.length}")

      var i = 1
      var max = a(0)
      while (i < length) {
        if (a(i) > max) max = a(i)
        i += 1
      }
      max
    }

    /**
     *
     */
    def activeMax(a: Array[Double], active: Array[Int], activeCount: Int) = {
      assert(activeCount != 0, s"Cannot compute activeMax for an activeCount of zero. (Active array has length ${active.length})")
      assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to activeMax, for an active array of length ${active.length}")
      assert(activeCount <= a.length, s"Passed in an activeCount of $activeCount to activeMax, for an array of length ${a.length}")

      var max = a(active(0))
      var i = 1
      while (i < activeCount) {
        if (a(active(i)) > max)
          max = a(active(i))
        i += 1
      }
      max
    }

    /**
     *
     */
    def argmax(a: Array[Double], length: Int) = {
      assert(length != 0, s"Cannot compute argmax for a length of zero. (Array has length ${a.length})")
      assert(length <= a.length, s"Passed in a length of $length to argmax, for an array of length ${a.length}")

      var max = a(0)
      var maxIdx = 0
      var i = 1
      while (i < length) {
        if (a(i) > max) {
          max = a(i)
          maxIdx = i
        }
        i += 1
      }
      maxIdx
    }

    /**
     *
     */
    def activeArgmax(a: Array[Double], active: Array[Int], activeCount: Int) = {
      assert(activeCount != 0, s"Cannot compute activeArgmax for an activeCount of zero. (Active array has length ${active.length})")
      assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to activeArgmax, for an active array of length ${active.length}")
      assert(activeCount <= a.length, s"Passed in an activeCount of $activeCount to activeArgmax, for an array of length ${a.length}")

      var maxIdx = active(0)
      var max = a(maxIdx)
      var i = 1
      while (i < activeCount) {
        if (a(active(i)) > max) {
          maxIdx = active(i)
          max = a(maxIdx)
        }
        i += 1
      }
      maxIdx
    }

    /**
     * sample from the distribution
     */
    def choose(dist: Array[Double], count: Int, rand: RandomGenerator): Int = {
      assert(count != 0, s"Cannot choose for a count of zero. (Array has length ${dist.length})")
      assert(count <= dist.length, s"Passed in a count of $count to choose, for an array of length ${dist.length}")

      if (count == 1) return 0
      val pSum = sum(dist, count)
      val r = rand.nextDouble
      var s = pSum * r
      var i = 0
      while (i < count) {
        s -= dist(i)
        if (s < 0) return i
        i += 1
      }
      assert(!pSum.isInfinite, f"in choose, pSum=$pSum")
      sys.error(f"No value chosen in choose!  ${dist.mkString("[", ", ", "]")}, r=$r%.2f")
    }

    /**
     * sample from the distribution
     */
    def activeChoose(dist: Array[Double], active: Array[Int], activeCount: Int, rand: RandomGenerator): Int = {
      assert(activeCount != 0, s"Cannot activeChoose for an activeCount of zero. (Active array has length ${active.length})")
      assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to activeChoose, for an active array of length ${active.length}")
      assert(activeCount <= dist.length, s"Passed in an activeCount of $activeCount to activeChoose, for a dist array of length ${dist.length}")

      if (activeCount == 1) return active(0)
      val pSum = activeSum(dist, active, activeCount)
      val r = rand.nextDouble
      var s = pSum * r
      var i = 0
      while (i < activeCount) {
        val ai = active(i)
        s -= dist(ai)
        if (s < 0) return ai
        i += 1
      }
      sys.error(f"No value chosen in activeChoose!  ${dist.mkString("[", ", ", "]")}, ${active.take(activeCount).mkString("[[", ", ", "]]")}${active.drop(activeCount).mkString("", " ", "]")}")
    }

    /**
     * Don't let all active values to be log(0)
     */
    def logChoose(logDist: Array[Double], count: Int, rand: RandomGenerator): Int = {
      assert(count != 0, s"Cannot logChoose for a count of zero. (Array has length ${logDist.length})")
      assert(count <= logDist.length, s"Passed in a count of $count to logChoose, for an array of length ${logDist.length}")

      if (count == 1) return 0
      val logProbSum = logSum(logDist, count)
      val r = rand.nextDouble
      var prob = r
      var i = 0
      while (i < count) {
        prob -= exp(logDist(i) - logProbSum)
        if (prob < 0) return i
        i += 1
      }
      assert(!logProbSum.isInfinite, f"in logChoose, logProbSum=$logProbSum")
      sys.error(f"No value chosen in logChoose!  ${logDist.mkString("[", ", ", "]")}, r=$r%.2f")
    }

    /**
     * Don't let all active values to be log(0)
     */
    def activeLogChoose(logDist: Array[Double], active: Array[Int], activeCount: Int, rand: RandomGenerator): Int = {
      assert(activeCount != 0, s"Cannot activeLogChoose for an activeCount of zero. (Active array has length ${active.length})")
      assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to activeLogChoose, for an active array of length ${active.length}")
      assert(activeCount <= logDist.length, s"Passed in an activeCount of $activeCount to activeLogChoose, for a logDist array of length ${logDist.length}")

      if (activeCount == 1) return active(0)
      val logSum = activeLogSum(logDist, active, activeCount)
      var prob = rand.nextDouble
      var i = 0
      while (i < activeCount) {
        val ai = active(i)
        prob -= exp(logDist(ai) - logSum)
        if (prob < 0) return ai
        i += 1
      }
      sys.error(f"No value chosen in activeLogChoose!  ${logDist.mkString("[", ", ", "]")}, ${active.take(activeCount).mkString("[[", ", ", "]]")}${active.drop(activeCount).mkString("", " ", "]")}")
    }

    /**
     * In-place normalization of elements up to length
     */
    def normalize(a: Array[Double], length: Int): Unit = {
      assert(length != 0, s"Cannot normalize for a length of zero. (Array has length ${a.length})")
      // length <= a.length
      var s = sum(a, length)
      var i = 0
      while (i < length) {
        a(i) /= s
        i += 1
      }
    }

    /**
     * In-place normalization of active elements
     */
    def activeNormalize(a: Array[Double], active: Array[Int], activeCount: Int): Unit = {
      assert(activeCount != 0, s"Cannot activeNormalize for an activeCount of zero. (Active array has length ${active.length})")
      // activeCount <= active.length
      // activeCount <= a.length
      var s = activeSum(a, active, activeCount)
      var i = 0
      while (i < activeCount) {
        val ai = active(i)
        a(ai) /= s
        i += 1
      }
    }

    /**
     * In-place normalization, then logging, of elements up to length
     */
    def normalizeAndLog(a: Array[Double], length: Int): Unit = {
      assert(length != 0, s"Cannot normalizeAndLog for a length of zero. (Array has length ${a.length})")
      // length <= a.length
      var s = sum(a, length)
      var i = 0
      while (i < length) {
        a(i) = log(a(i) / s)
        i += 1
      }
    }

    /**
     * In-place normalization, then logging, of active elements
     */
    def activeNormalizeAndLog(a: Array[Double], active: Array[Int], activeCount: Int): Unit = {
      assert(activeCount != 0, s"Cannot activeNormalizeAndLog for an activeCount of zero. (Active array has length ${active.length})")
      // activeCount <= active.length
      // activeCount <= a.length
      var s = activeSum(a, active, activeCount)
      var i = 0
      while (i < activeCount) {
        val ai = active(i)
        a(ai) = log(a(ai) / s)
        i += 1
      }
    }

    /**
     * In-place normalization of log-valued elements up to length
     */
    def logNormalize(logData: Array[Double], length: Int): Unit = {
      assert(length != 0, s"Cannot logNormalize for a length of zero. (Array has length ${logData.length})")
      // length <= a.length
      var logsum = logSum(logData, length)
      var i = 0
      while (i < length) {
        logData(i) = logData(i) - logsum
        i += 1
      }
    }

    /**
     * In-place normalization of active log-valued elements
     */
    def activeLogNormalize(logData: Array[Double], active: Array[Int], activeCount: Int): Unit = {
      assert(activeCount != 0, s"Cannot activeLogNormalize for an activeCount of zero. (Active array has length ${active.length})")
      // activeCount <= active.length
      // activeCount <= a.length
      var logSum = activeLogSum(logData, active, activeCount)
      var i = 0
      while (i < activeCount) {
        val ai = active(i)
        logData(ai) = logData(ai) - logSum
        i += 1
      }
    }

    /**
     * makes log-normalized probabilities
     */
    def convertToLogDirichletDraw(counts: Array[Double], length: Int, rand: RandomGenerator): Unit = {
      assert(length != 0, s"Cannot convertToLogDirichletDraw for a length of zero. (Array has length ${counts.length})")
      assert(length <= counts.length, s"Passed in a length of $length to convertToLogDirichletDraw, for an array of length ${counts.length}")

      var i = 0
      while (i < length) {
        val gd = gammaLogDraw(counts(i), rand)
        counts(i) = gd
        i += 1
      }
      logNormalize(counts, length)
    }

    /**
     * makes log-normalized probabilities
     */
    def convertActiveToLogDirichletDraw(counts: Array[Double], active: Array[Int], activeCount: Int, rand: RandomGenerator): Unit = {
      assert(activeCount != 0, s"Cannot convertActiveToLogDirichletDraw for an activeCount of zero. (Active array has length ${active.length})")
      assert(activeCount <= active.length, s"Passed in an activeCount of $activeCount to convertActiveToLogDirichletDraw, for an active array of length ${active.length}")
      assert(activeCount <= counts.length, s"Passed in an activeCount of $activeCount to convertActiveToLogDirichletDraw, for an array of length ${counts.length}")

      var i = 0
      while (i < activeCount) {
        val ai = active(i)
        val gd = gammaLogDraw(counts(ai), rand)
        counts(ai) = gd
        i += 1
      }
      activeLogNormalize(counts, active, activeCount)
    }

    def gammaLogDraw(shape: Double, rand: RandomGenerator): Double = {
      if (shape < 1) {
        // adapted from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
        @tailrec
        def rec: Double = {
          val u = rand.nextDouble
          val v = -math.log(rand.nextDouble)
          val logU = log(u)
          if (logU <= math.log1p(-shape)) {
            val logV = log(v)
            val logX = logU / shape
            if (logX <= logV) logX
            else rec
          }
          else {
            val y = -log((1 - u) / shape)
            val logX = math.log(1.0 - shape + shape * y) / shape
            if (logX <= math.log(v + y)) logX
            else rec
          }
        }
        rec
      }
      else math.log(gammaDraw(shape, rand))
    }

    def gammaDraw(shape: Double, rand: RandomGenerator): Double = {
      if (shape == 1.0) {
        -math.log(rand.nextDouble)
      }
      else if (shape < 1.0) {
        // from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
        @tailrec
        def rec: Double = {
          val u = rand.nextDouble
          val v = -math.log(rand.nextDouble)
          if (u <= 1.0 - shape) {
            val x = pow(u, 1.0 / shape)
            if (x <= v) x
            else rec
          }
          else {
            val y = -log((1 - u) / shape)
            val x = pow(1.0 - shape + shape * y, 1.0 / shape)
            if (x <= (v + y)) x
            else rec
          }
        }
        rec
      }
      else {
        // from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
        val d = shape - 1.0 / 3.0
        val c = 1.0 / math.sqrt(9.0 * d)
        var r = 0.0
        var ok = false
        while (!ok) {
          var v = 0.0
          var x = 0.0
          do {
            x = rand.nextGaussian
            v = 1.0 + c * x
          } while (v <= 0)

          v = v * v * v
          val x2 = x * x
          val u = rand.nextDouble
          if (u < 1.0 - 0.0331 * (x2 * x2)
            || log(u) < 0.5 * x2 + d * (1.0 - v + log(v))) {
            r = d * v
            ok = true
          }
        }
        r
      }
    }

  }

  //////////////////////////////////
  // TestUtil.scala
  //////////////////////////////////

  object TestUtil {

    private[this] class TestUtilExceptionNotFound extends RuntimeException("exception expected, but no exception thrown")

    def assertEqualsDouble(expected: Double, actual: Double) {
      assertEquals(expected, actual, 0.000000001)
    }

    def assertException(block: => Unit)(handle: PartialFunction[Throwable, Unit]) {
      try { block; throw new TestUtilExceptionNotFound } catch {
        case e: TestUtilExceptionNotFound => fail(e.getMessage)
        case e: Throwable => handle(e)
      }
    }

    def assertExceptionAny(block: => Unit) {
      try { block; throw new TestUtilExceptionNotFound } catch {
        case e: TestUtilExceptionNotFound => fail(e.getMessage)
        case e: Throwable =>
      }
    }

    def assertExceptionMsg(expectedMessage: String)(block: => Unit) {
      try { block; throw new TestUtilExceptionNotFound } catch {
        case e: TestUtilExceptionNotFound => fail(e.getMessage)
        case e: Throwable => assertEquals(expectedMessage, e.getMessage)
      }
    }

    def assertExceptionMsg(expectedMessageRe: Regex)(block: => Unit) {
      try { block; throw new TestUtilExceptionNotFound } catch {
        case e: TestUtilExceptionNotFound => fail(e.getMessage)
        case e: Throwable => assertMatch(expectedMessageRe, e.getMessage)
      }
    }

    def assertMatch(expectedRe: Regex, result: String) = {
      if (!expectedRe.matches(result))
        throw new AssertionError(s"expected match: $expectedRe but was: $result")
    }

    def assertEqualsArray[A](expected: Array[A], result: Array[A]) {
      if (expected.size != result.size || (expected zip result).exists(p => p._1 != p._2))
        throw new AssertionError(s"expected: Array(${expected.mkString(",")}) but was: Array(${result.mkString(",")})")
    }

    def assertEqualsIterator[A](expected: Iterator[A], result: Iterator[A]) {
      var i = 0
      while (expected.hasNext && result.hasNext) {
        assertEquals("mismatch on element " + i, expected.next, result.next)
        i += 1
      }
      if (expected.hasNext)
        fail("expected still contains: [%s]".format(expected.toSeq.mkString(", ")))
      if (result.hasNext)
        fail("result still contains: [%s]".format(result.toSeq.mkString(", ")))
    }

    def assertEqualsSameElements[T: Ordering](expected: Seq[T], actual: Seq[T]) {
      assertEquals("%s vs %s: DIFFERENCE: (%s)".format(expected, actual, (expected ++ actual).toSet -- (expected.toSet & actual.toSet)), expected.sorted.size, actual.sorted.size)
      for ((e, a) <- expected.sorted zipSafe actual.sorted)
        assertEquals(e, a)
    }

    def assertEqualsSmart[T](expected: (T, Double), actual: (T, Double)) {
      assertEquals(expected._1, actual._1)
      assertEqualsDouble(expected._2, actual._2)
    }

    def assertEqualsSmartLog[T](expected: (T, Double), actual: (T, LogDouble)) {
      assertEquals(expected._1, actual._1)
      assertEqualsDouble(log(expected._2), actual._2.logValue)
    }

    def assertEqualsSmart[T](expected: Double, actual: LogDouble) {
      assertEqualsDouble(log(expected), actual.logValue)
    }

    def assertEqualsSmart[T](expected: Map[T, Double], actual: Map[T, Double]) {
      def keystr(m: Map[T, Double]) = s"${m.keys.toVector.map(_.toString).sorted.mkString(", ")}"
      assertEquals("Wrong keys.", keystr(expected), keystr(actual))
      for ((k, ev) <- expected)
        assertEqualsDouble(ev, actual(k))
    }

    def assertEqualsSmartLog[T](expected: Map[T, Double], actual: Map[T, LogDouble]) {
      def keystr(m: Map[T, _]) = s"${m.keys.toVector.map(_.toString).sorted.mkString(", ")}"
      assertEquals("Wrong keys.", keystr(expected), keystr(actual))
      for ((k, ev) <- expected)
        assertEqualsDouble(log(ev), actual(k).logValue)
    }

    def assertEqualsSmart[A](expected: Option[Map[A, Double]], actual: Option[Map[A, Double]]) {
      assertEquals(expected.isDefined, actual.isDefined)
      if (expected.isDefined) assertEqualsSmart(expected.get, actual.get)
    }

    def assertEqualsSmart[A, B](expected: Vector[(A, Map[B, Double])], actual: Vector[(A, Map[B, Double])]) {
      assertEquals(expected.size, actual.size)
      for (((eA, eB), (aA, aB)) <- expected zip actual) {
        assertEquals(eA, aA)
        assertEqualsSmart(eB, aB)
      }
    }

    //  def dumpToFile(lines: TraversableOnce[String]): String = {
    //    val tempFile = mktemp("tmp")
    //    writeUsing(tempFile) { f => lines.foreach(l => f.write(l + "\n")) }
    //    tempFile.path
    //  }

    //  def dumpToFile(lines: String): String =
    //    dumpToFile(lines.split("\n").map(_.trim).filter(_.nonEmpty))

    //  def printFile(filename: String) =
    //    File(filename).readLines.foreach(println)

    //  def readFile(filename: String) =
    //    File(filename).readLines.mkString("\n")

    //  def assertFile(expected: String, filename: String) = {
    //    val e = expected.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n")
    //    val f = readFile(filename).split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n")
    //    //e.zip(f).zipWithIndex.foreach { case ((a, b), i) => assertEquals(a, b) } //"char at [%d]: [%s] vs [%s]".format(i,a,b)
    //    assertEquals(e, f)
    //  }

    //  def assertPath(expected: String, filename: String) =
    //    assertEquals(File(expected).getAbsolutePath, File(filename).getAbsolutePath)

    trait MockableRandomGenerator extends RandomGenerator {
      override def setSeed(seed: Int): Unit = ???
      override def setSeed(seed: Array[Int]): Unit = ???
      override def setSeed(seed: Long): Unit = ???
      override def nextBytes(bytes: Array[Byte]): Unit = ???
      override def nextInt(): Int = ???
      override def nextInt(n: Int): Int = ???
      override def nextLong(): Long = ???
      override def nextBoolean(): Boolean = ???
      override def nextFloat(): Float = ???
      override def nextDouble(): Double = ???
      override def nextGaussian(): Double = ???
    }

    case class DoubleIteratorRandomGenerator(it: Iterator[Double]) extends MockableRandomGenerator {
      override def nextDouble() = it.next()
    }

  }

  //////////////////////////////////
  // GraphUtil
  //////////////////////////////////

  object GraphUtil {

    def toDagOrder[A](allLinks: Set[(A, A)]): Vector[A] = toDagOrder(allLinks, allLinks.flatMap { case (a, b) => Set(a, b) })
    def toDagOrder[A](allLinks: Set[(A, A)], allElements: Set[A]): Vector[A] = {
      def rec[A](remainingLinks: Set[(A, A)], remainingElements: Set[A]): Vector[A] = {
        if (remainingLinks.isEmpty) remainingElements.toVector
        else {
          val haveIncomingLink = remainingLinks.map(_._2)
          val noIncoming = (remainingElements -- haveIncomingLink) // find elements with no incoming links
          //println(f"dagOrder: reminaingLinks=$remainingLinks, remainingElements=$remainingElements, haveIncomingLink=$haveIncomingLink, noIncoming=$noIncoming")
          noIncoming.toVector ++ rec(remainingLinks.filterNot { case (a, b) => noIncoming(a) }, remainingElements -- noIncoming) // delete any outgoing links from e (thus removing it from the graph entirely), and recurse
        }
      }
      assert(!hasCycles(allLinks), "Cannot compute dagOrder due to cycle!")
      rec(allLinks, allElements | allLinks.flatMap { case (a, b) => Set(a, b) })
    }

    @tailrec final def hasCycles[A](allLinks: Set[(A, A)]): Boolean = {
      allLinks match {
        case SetHeadTail((a, b), otherLinks) =>
          if (allLinks(b -> a)) true
          else {
            // replace 'b' links with 'a's
            hasCycles(otherLinks ++ otherLinks.collect { case (`b`, c) => a -> c })
          }
        case _ => false
      }
    }

    def findCycles[A](allLinks: Set[(A, A)]): Set[Vector[A]] = {
      if (hasCycles(allLinks)) {
        val edgeMap = allLinks.groupByKey.withDefaultValue(Set.empty)
        def inner(path: Vector[A]): Set[Vector[A]] = {
          val nexts = edgeMap(path.last)
          nexts.flatMap { n =>
            val nIdx = path.indexOf(n)
            if (nIdx == -1) inner(path :+ n)
            else Set(path.drop(nIdx))
          }
        }
        val allNodes = allLinks.flatMap { case (a, b) => Set(a, b) }
        allNodes.flatMap { n =>
          inner(Vector(n))
        }
      }
      else {
        Set.empty
      }
    }

    def findUniqueCycles[A](allLinks: Set[(A, A)]): Set[Vector[A]] = {
      findCycles(allLinks).map { path =>
        path.toSet -> path
      }.toMap.values.toSet
    }

  }

  //////////////////////////////////
  // viz package
  //////////////////////////////////

  object viz {

    //////////////////////////////////
    // Dataset.scala
    //////////////////////////////////

    /**
     * Factories for constructing datasets for charts
     *
     * @author Dan Garrette (dhgarrette@gmail.com)
     */

    object XYDataset {
      def apply(xyPairs: TraversableOnce[(Double, Double)], name: String = "") = {
        val series1 = new XYSeries(name)
        for ((x, y) <- xyPairs)
          series1.add(x, y)
        val collection1 = new XYSeriesCollection()
        collection1.addSeries(series1)
        collection1
      }
    }

    object LineGraphDataset {
      def apply(xyPairs: TraversableOnce[(Double, Double)], name: String = "") = XYDataset(xyPairs, name)
    }

    case class HistogramDatasetBuilder private (
        data: Option[GenTraversable[Double]] = None,
        numBins: Option[Int] = None,
        rangeStart: Option[Double] = None,
        rangeEnd: Option[Double] = None,
        binWidth: Option[Double] = None) {

      def withData(data: GenTraversable[Double]) = this.copy(data = Some(data))
      def withNumBins(numBins: Int) = this.copy(numBins = Some(numBins))
      def withRangeStart(rangeStart: Double) = this.copy(rangeStart = Some(rangeStart))
      def withRangeEnd(rangeEnd: Double) = this.copy(rangeEnd = Some(rangeEnd))
      def withBinWidth(binWidth: Double) = this.copy(binWidth = Some(binWidth))

      def d(data: GenTraversable[Double]) = withData(data)
      def n(numBins: Int) = withNumBins(numBins)
      def s(rangeStart: Double) = withRangeStart(rangeStart)
      def e(rangeEnd: Double) = withRangeEnd(rangeEnd)
      def w(binWidth: Double) = withBinWidth(binWidth)

      def build: HistogramDataset = {
        assert(data.isDefined)
        this match {
          case HistogramDatasetBuilder(Some(data), Some(numBins), None, None, None) => this.withRangeStart(data.min).withRangeEnd(data.max).build
          case HistogramDatasetBuilder(Some(data), Some(numBins), Some(rangeStart), None, None) => this.withRangeEnd(data.max).build
          case HistogramDatasetBuilder(Some(data), Some(numBins), None, Some(rangeEnd), None) => this.withRangeStart(data.min).build
          case HistogramDatasetBuilder(Some(data), Some(numBins), Some(rangeStart), Some(rangeEnd), None) =>
            val binWidth = (rangeEnd - rangeStart) / numBins
            HistogramDataset.make(data, numBins, rangeStart, binWidth)
          case HistogramDatasetBuilder(Some(data), None, None, None, Some(binWidth)) => this.withRangeStart(data.min).withRangeEnd(data.max).build
          case HistogramDatasetBuilder(Some(data), None, Some(rangeStart), None, Some(binWidth)) => this.withRangeEnd(data.max).build
          case HistogramDatasetBuilder(Some(data), None, None, Some(rangeEnd), Some(binWidth)) => this.withRangeStart(data.min).build
          case HistogramDatasetBuilder(Some(data), None, Some(rangeStart), Some(rangeEnd), Some(binWidth)) =>
            val numBins = ((rangeEnd - rangeStart) / binWidth).toInt + 1
            HistogramDataset.make(data, numBins, rangeStart, binWidth)
          case HistogramDatasetBuilder(Some(data), Some(numBins), None, Some(rangeEnd), Some(binWidth)) =>
            val rangeStart = rangeEnd - (numBins * binWidth)
            HistogramDataset.make(data, numBins, rangeStart, binWidth)
          case HistogramDatasetBuilder(Some(data), Some(numBins), Some(rangeStart), None, Some(binWidth)) =>
            HistogramDataset.make(data, numBins, rangeStart, binWidth)
          case HistogramDatasetBuilder(Some(data), Some(numBins), Some(rangeStart), Some(rangeEnd), Some(binWidth)) =>
            sys.error("Cannot specify numBins, rangeStart, rangeEnd, and binWidth together.")
        }
      }

      implicit def toDataset(hdb: HistogramDatasetBuilder) = hdb.build.dataset
    }
    object HistogramDatasetBuilder {
      def apply() = new HistogramDatasetBuilder()
      def apply(data: GenTraversable[Double]) = new HistogramDatasetBuilder(data = Some(data))
    }

    object HistogramDataset {
      def apply(data: GenTraversable[Double], numBins: Int): HistogramDataset = {
        val rangeStart = data.min
        val rangeEnd = data.max
        make(data, numBins, rangeStart, binWidth(numBins, rangeStart, rangeEnd))
      }

      def byWidth(data: GenTraversable[Double], binWidth: Double): HistogramDataset = {
        val rangeStart = data.min
        val rangeEnd = data.max
        val numBins = ((rangeEnd - rangeStart) / binWidth).toInt + 1
        make(data, numBins, rangeStart, binWidth)
      }

      def make(data: GenTraversableOnce[Double], numBins: Int, rangeStart: Double, binWidth: Double) = {
        val binArray = makeBinArray(data, numBins, rangeStart, binWidth)
        HistogramDataset(binArray, rangeStart, binWidth)
      }

      def byStartEnd(data: GenTraversableOnce[Double], numBins: Int, rangeStart: Double, rangeEnd: Double) = {
        val binwidth = binWidth(numBins, rangeStart, rangeEnd)
        val binArray = makeBinArray(data, numBins, rangeStart, binwidth)
        HistogramDataset(binArray, rangeStart, binwidth)
      }

      def binWidth(numBins: Int, rangeStart: Double, rangeEnd: Double) = (rangeEnd - rangeStart) / numBins

      def binData[A](data: GenTraversableOnce[(A, Double)], numBins: Int, rangeStart: Double, binWidth: Double): Vector[Vector[A]] = {
        val bins = Vector.fill(numBins)(Vector.newBuilder[A])
        data.foreach {
          case (a, t) =>
            val b = ((t - rangeStart) / binWidth).toInt
            val bin = if (b == numBins) b - 1 else b
            bins(bin) += a
        }
        bins.map(_.result)
      }

      def makeBinArray(data: GenTraversableOnce[Double], numBins: Int, rangeStart: Double, binWidth: Double): Vector[Int] = {
        binData(data.toIterator.map(t => (t, t)), numBins, rangeStart, binWidth).map(_.size)
      }

      implicit def toDataset(hd: HistogramDataset) = hd.dataset
    }

    case class HistogramDataset(binArray: Vector[Int], rangeStart: Double, binWidth: Double) {

      val numBins = binArray.size
      val rangeEnd = rangeStart + numBins * binWidth

      def dataset = {
        val halfBinWidth = binWidth / 2
        val xyPairs = binArray.zipWithIndex.map { case (count, i) => (rangeStart + i * binWidth, count.toDouble) }
        val series1 = new XYIntervalSeries("")
        for ((x, y) <- xyPairs)
          series1.add(x + halfBinWidth, x, x + binWidth, y, 0, y)
        val collection1 = new XYIntervalSeriesCollection()
        collection1.addSeries(series1)
        collection1
      }

    }

    case class SingleHistogramBarDataset(count: Int, binNumber: Int, numBins: Int, rangeStart: Double, binWidth: Double) {

      def dataset = {
        val halfBinWidth = binWidth / 2
        val x = rangeStart + binNumber * binWidth
        val y = count.toDouble
        val series1 = new XYIntervalSeries("")
        series1.add(x + halfBinWidth, x, x + binWidth, y, 0, y)
        val collection1 = new XYIntervalSeriesCollection()
        collection1.addSeries(series1)
        collection1
      }

    }

    //////////////////////////////////
    // VizChart.scala
    //////////////////////////////////

    /**
     * A chart for visualizing data
     *
     * @author Dan Garrette (dhgarrette@gmail.com)
     */
    trait VizChart {
      def allCharts: Vector[SingleChart]

      def draw(
        a: Int = 10, b: Int = 10,
        width: Int = 800, height: Int = 500,
        title: String = "",
        xaxisLabel: String = "",
        yaxisLabel: String = "",
        showLegend: Boolean = false,
        includeXZero: Boolean = true,
        includeYZero: Boolean = true,
        xaxisDates: Boolean = false,
        exitOnClose: Boolean = true) {

        val plot = new XYPlot()

        val xaxis = if (xaxisDates) {
          new DateAxis(xaxisLabel)
        }
        else {
          val a = new NumberAxis(xaxisLabel)
          a.setAutoRangeIncludesZero(includeXZero)
          a
        }

        val yaxis = new NumberAxis(yaxisLabel)
        yaxis.setAutoRangeIncludesZero(includeYZero)

        plot.setDomainAxis(0, xaxis)
        plot.setRangeAxis(0, yaxis)
        plot.setDatasetRenderingOrder(DatasetRenderingOrder.FORWARD)

        for ((SingleChart(dataset, renderer), i) <- allCharts.zipWithIndex) {
          plot.setDataset(i, dataset)
          plot.setRenderer(i, renderer)
          plot.mapDatasetToDomainAxis(i, 0)
          plot.mapDatasetToRangeAxis(i, 0)
        }

        val chart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, showLegend)

        val frame = new JFrame(title)
        frame.setContentPane(new ChartPanel(chart))

        //frame.pack()
        frame.setBounds(a, b, width, height)
        frame.setVisible(true)
        if (exitOnClose) frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      }
    }

    case class SingleChart(
      dataset: JXYDataset,
      renderer: XYItemRenderer)
        extends VizChart {
      def allCharts = Vector(this)
    }

    case class MultiChart(
      charts: Vector[VizChart])
        extends VizChart {
      def allCharts = charts.flatMap(_.allCharts)
    }

    object VizChart {
      def apply(
        dataset: JXYDataset,
        renderer: XYItemRenderer) = {
        SingleChart(dataset, renderer)
      }

      def apply(
        charts: VizChart*) = {
        MultiChart(charts.toVector)
      }

      def apply(
        charts: GenTraversableOnce[VizChart]) = {
        MultiChart(charts.toVector)
      }
    }

    //
    //
    //

    object BarChart {
      def apply[A](data: TraversableOnce[(A, Double)],
        color: Color = Color.blue): VizChart = {
        ???
      }

      def indexed(
        data: TraversableOnce[Double],
        color: Color = Color.blue): VizChart = {
        SingleChart(XYDataset(data.toIterator.zipWithIndex.map { case (x, i) => i.toDouble -> x }), BarRenderer(color))
      }
    }

    object Histogram {
      def apply(
        data: Vector[Double],
        numBins: Int,
        color: Color = Color.blue): VizChart = {
        SingleChart(HistogramDataset(data, numBins), BarRenderer(color))
      }
    }

    object ShadedHistogram {
      def apply(
        data: Vector[Double],
        darkness: Vector[Double], // values 0.0 to 1.0, one for each bin
        color: Color = Color.blue): VizChart = {
        val numBins = darkness.size
        fromHistDataset(HistogramDataset(data, numBins), darkness, color)
      }

      def fromHistDataset(
        histData: HistogramDataset,
        darkness: Vector[Double], // values 0.0 to 1.0, one for each bin
        color: Color = Color.blue) = {
        val numBins = histData.numBins
        assert(numBins == darkness.size, "histogram data and darkness vector are not the same size.")
        VizChart((histData.binArray zip darkness).zipWithIndex.map {
          case ((count, dark), binNumber) =>
            val dataset = new SingleHistogramBarDataset(count, binNumber, numBins, histData.rangeStart, histData.binWidth)
            val Array(r, g, b) = color.getRGBColorComponents(null).map(v => v + (1 - v) * (1 - dark.toFloat))
            val newColor = new Color(r, g, b)
            SingleChart(dataset.dataset, BarRenderer(newColor))
        })
      }
    }

    object LineGraph {
      def apply(
        data: TraversableOnce[(Double, Double)],
        color: Color = Color.blue,
        lineThickness: Int = 2,
        shape: Option[JShape] = None): VizChart = {
        SingleChart(LineGraphDataset(data), LineRenderer(color = color, lineThickness = lineThickness))
      }

      def makeIndexed(
        data: TraversableOnce[Double],
        color: Color = Color.blue,
        lineThickness: Int = 2,
        shape: Option[JShape] = None): VizChart = {
        apply(data.toIterator.zipWithIndex.map { case (y, x) => x.toDouble -> y }, color, lineThickness)
      }
    }

    object ScatterGraph {
      def apply(
        data: TraversableOnce[(Double, Double)],
        color: Color = Color.blue,
        shape: JShape = Shape.circle): VizChart = {
        SingleChart(XYDataset(data), ScatterRenderer(color = color, shape = shape))
      }

      def indexed(
        data: TraversableOnce[Double],
        color: Color = Color.blue,
        shape: JShape = Shape.circle): VizChart = {
        apply(data.toIterator.zipWithIndex.map { case (y, x) => x.toDouble -> y }, color, shape)
      }
    }

    //////////////////////////////////
    // Renderer.scala
    //////////////////////////////////

    /**
     * Factories for constructing renderers for charts
     *
     * @author Dan Garrette (dhgarrette@gmail.com)
     */

    object BarRenderer {
      def apply(
        color: Color = Color.blue) = {
        val r = new XYBarRenderer
        r.setBarPainter(new StandardXYBarPainter())
        r.setShadowVisible(false)
        r.setSeriesPaint(0, color)
        r
      }
    }

    object LineRenderer {
      def apply(
        color: Color = Color.blue,
        lineThickness: Int = 2,
        shape: Option[JShape] = None) = {
        require(lineThickness > 0 || shape.isDefined)
        val r = new XYLineAndShapeRenderer(lineThickness > 0, shape.isDefined)
        r.setSeriesPaint(0, color)
        r.setSeriesStroke(0, new BasicStroke(lineThickness))
        shape.foreach(s => r.setSeriesShape(0, s))
        r
      }
    }

    object ScatterRenderer {
      def apply(
        color: Color = Color.blue,
        shape: JShape = Shape.circle) = {
        LineRenderer(color, 0, Some(shape))
      }
    }

    object InvisibleXyRenderer {
      def apply() = new XYLineAndShapeRenderer(false, false)
    }

    //
    //
    //

    object Shape {
      def circle: JShape = circle(5)
      def circle(size: Double) = new Ellipse2D.Double(-size / 2, -size / 2, size, size)
    }

    //////////////////////////////////
    // Tree.scala
    //////////////////////////////////

    trait VizTree {
      def label: String
      def children: Vector[VizTree]

      final def pretty: String = prettyLines.mkString("\n")
      private def prettyLines: Vector[String] = {
        children.flatMap(_.prettyLines) match {
          case Vector(childLine) => Vector(label + " " + childLine)
          case childLines => label +: childLines.map("  " + _)
        }
      }
    }

    object TreeViz {

      //case class NodeObj(label: String, index: Int)
      type NodeObj = String
      def NodeObj(label: String) = label

      private[this] class TreeScene(root: NodeObj) extends GraphScene[NodeObj, String] {

        private[this] val mainLayer = new LayerWidget(this)
        private[this] val connectionLayer = new LayerWidget(this)

        addChild(mainLayer)
        addChild(connectionLayer)

        def addEdge(a: NodeObj, b: NodeObj): Unit = {
          addEdge(a + "->" + b)
          setEdgeSource(a + "->" + b, a)
          setEdgeTarget(a + "->" + b, b)
        }

        def attachNodeWidget(n: NodeObj): Widget = {
          val w = new LabelWidget(this)
          w.setLabel(" " + n + " ")
          mainLayer.addChild(w)
          w
        }

        def attachEdgeWidget(e: String): Widget = {
          val connectionWidget = new ConnectionWidget(this)
          //connectionWidget.setTargetAnchorShape(AnchorShape.TRIANGLE_FILLED)
          connectionLayer.addChild(connectionWidget)
          connectionWidget
        }

        def attachEdgeSourceAnchor(edge: String, oldSourceNode: NodeObj, sourceNode: NodeObj): Unit = {
          val edgeWidget = findWidget(edge).asInstanceOf[ConnectionWidget]
          val sourceNodeWidget = findWidget(sourceNode)
          val sourceAnchor = AnchorFactory.createRectangularAnchor(sourceNodeWidget)
          edgeWidget.setSourceAnchor(sourceAnchor)
        }

        def attachEdgeTargetAnchor(edge: String, oldTargetNode: NodeObj, targetNode: NodeObj): Unit = {
          val edgeWidget = findWidget(edge).asInstanceOf[ConnectionWidget]
          val targetNodeWidget = findWidget(targetNode)
          val targetAnchor = AnchorFactory.createRectangularAnchor(targetNodeWidget)
          edgeWidget.setTargetAnchor(targetAnchor)
        }
      }

      private[this] def showScene(scene: TreeScene, title: String): Unit = {
        val panel = new JScrollPane(scene.createView())
        val dialog = new JDialog()
        dialog.setModal(true)
        dialog.setTitle(title)
        dialog.add(panel, BorderLayout.CENTER)
        dialog.setSize(800, 600)
        dialog.setVisible(true)
        dialog.dispose()
      }

      private[this] def layoutScene(scene: GraphScene[NodeObj, String], root: NodeObj): Unit = {
        val graphLayout = new AbegoTreeLayoutForNetbeans[NodeObj, String](root, 100, 100, 50, 50, true)
        val sceneLayout = LayoutFactory.createSceneGraphLayout(scene, graphLayout)
        sceneLayout.invokeLayoutImmediately()
      }

      private[this] class Counter(start: Int = 0) {
        private[this] var i = start
        def get = { val c = i; i += 1; c }
      }

      private[this] def createScene(t: VizTree): TreeScene = {
        val scene = new TreeScene(NodeObj(t.label))
        def addTree(t: VizTree): Unit = {
          scene.addNode(NodeObj(t.label))
          for (c <- t.children) {
            addTree(c)
            scene.addEdge(NodeObj(t.label), NodeObj(c.label))
          }
        }
        addTree(t)
        scene
      }

      def drawTree(t: VizTree): Unit = {
        val counter = new Counter
        def relabel(t: VizTree): (VizTree, (Int, Int)) = {
          if (t.children.isEmpty) {
            val i = counter.get
            (new VizTree { val label = f"${i}_${t.label}"; val children = Vector() }, (i, i))
          }
          else {
            val (newchildren, indexPairs) = t.children.map(relabel).unzip
            val (i, j) = (indexPairs.map(_._1).min, indexPairs.map(_._2).max)
            (new VizTree { val label = f"${i}-${j}_${t.label}"; val children = newchildren }, (i, j))
          }
        }

        val t2 = relabel(t)._1
        val s = createScene(t2)
        layoutScene(s, NodeObj(t2.label))
        showScene(s, "")
      }

    }
  }

}
