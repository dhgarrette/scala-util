package dhg.util.viz

import dhg.util.CollectionUtil._
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

case class HistogramDatasetBuilder(
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
