package dhg.util.viz

import dhg.util.CollectionUtil._
import java.lang.Comparable
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.data.xy.XYSeries
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.statistics.{ HistogramDataset => JfcHistogramDataset }

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
  def apply(xyPairs: TraversableOnce[(Double, Double)], name: String) = XYDataset(xyPairs, name)
}

object BarChartDataset {
  def multiRow(data: TraversableOnce[(Comparable[_], Comparable[_], Double)], name: String = "") = {
    val dataset: DefaultCategoryDataset = new DefaultCategoryDataset()
    for ((rowKey, columnKey, value) <- data)
      dataset.addValue(value, rowKey, columnKey)
    dataset
  }

  def apply(data: TraversableOnce[(Comparable[_], Double)], name: String = "") = {
    multiRow(data.map { case (columnKey, value) => ("<none>", columnKey, value) })
  }
}

object HistogramDataset {
  def apply(data: TraversableOnce[Double], bins: Int, name: String = "") = {
    val dataset = new JfcHistogramDataset
    dataset.addSeries("", data.toArray, bins, 0, 1);
    dataset
  }
}

object FakeHistogramDataset {
  def withBinNames(data: TraversableOnce[Double], bins: Vector[String], name: String = "") = {
    val (binArray, _) = makeBinArray(data, bins.size)
    new FakeHistogramDataset(binArray, bins)
  }

  def apply(data: TraversableOnce[Double], numBins: Int, name: String = "") = {
    val (binArray, binSize) = makeBinArray(data, numBins)
    val binNames = Vector.tabulate(numBins)(i => f"${i * binSize}%.2f")
    new FakeHistogramDataset(binArray, binNames)
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

  implicit def toCategoryDataset(fhd: FakeHistogramDataset) = fhd.toCategoryDataset(fhd)
}

class FakeHistogramDataset(val binArray: Array[Int], val bins: Vector[String], val name: String = "") {
  def toCategoryDataset(fhd: FakeHistogramDataset) = {
    val dataset: DefaultCategoryDataset = new DefaultCategoryDataset()
    for ((binName, count) <- bins zipSafe binArray)
      dataset.addValue(count, "<none>", binName)
    dataset
  }
}

object FakeHistogramXYDataset {
  def apply(data: TraversableOnce[Double], numBins: Int, name: String = "") = {
    val (binArray, binSize) = makeBinArray(data, numBins)
    XYDataset(binArray.zipWithIndex.map { case (count, i) => (i * binSize, count.toDouble) })
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
}
