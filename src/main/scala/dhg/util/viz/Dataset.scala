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

object HistogramDataset {
  def apply(data: TraversableOnce[Double], numBins: Int, name: String = "") = {
    val (binArray, binSize) = makeBinArray(data, numBins)
    new HistogramDataset(binArray, numBins, binSize)
  }

  def makeBinArray(data: TraversableOnce[Double], numBins: Int) = {
    val min = data.min
    val binSize = data.max / numBins
    val binArray = Array.fill(numBins)(0)
    for (t <- data) {
      val b = ((t - min) / binSize).toInt
      val bin = if (b == numBins) b - 1 else b
      binArray(bin) += 1
    }
    //for ((count, i) <- binArray.zipWithIndex) println(f"$i%4d: $count")
    (binArray, binSize)
  }

  implicit def toDataset(hd: HistogramDataset) = hd.dataset
}

class HistogramDataset(val binArray: Array[Int], val numBins: Int, val binSize: Double) {

  def dataset = {
    val halfBinSize = binSize / 2
    val xyPairs = binArray.zipWithIndex.map { case (count, i) => (i * binSize, count.toDouble) }

    val series1 = new XYIntervalSeries("")
    for ((x, y) <- xyPairs)
      series1.add(x, x - halfBinSize, x + halfBinSize, y, 0, 0)
    val collection1 = new XYIntervalSeriesCollection()
    collection1.addSeries(series1)
    collection1
  }

}
