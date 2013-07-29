package dhg.util.viz

import dhg.util.CollectionUtil._
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

/**
 * A chart for visualizing data
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
trait Chart {
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
    xaxisDates: Boolean = false) {

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
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  }
}

case class SingleChart(
  dataset: JXYDataset,
  renderer: XYItemRenderer)
  extends Chart {
  def allCharts = Vector(this)
}

case class MultiChart(
  charts: Vector[Chart])
  extends Chart {
  def allCharts = charts.flatMap(_.allCharts)
}

object Chart {
  def apply(
    dataset: JXYDataset,
    renderer: XYItemRenderer) = {
    SingleChart(dataset, renderer)
  }

  def apply(
    charts: Chart*) = {
    MultiChart(charts.toVector)
  }

  def apply(
    charts: GenTraversable[Chart]) = {
    MultiChart(charts.toVector)
  }
}

//
//
//

object BarChart {
  def make[A](data: Vector[(A, Double)],
    color: Color = Color.blue): Chart = {
    ???
  }

  def makeIndexed(
    data: Vector[Double],
    color: Color = Color.blue): Chart = {
    SingleChart(XYDataset(data.zipWithIndex.map { case (x, i) => (i.toDouble, x) }), BarRenderer(color))
  }
}

object Histogram {
  def make(
    data: Vector[Double],
    numBins: Int,
    color: Color = Color.blue): Chart = {
    SingleChart(HistogramDataset(data, numBins), BarRenderer(color))
  }
}

object ShadedHistogram {
  def make(
    data: Vector[Double],
    darkness: Vector[Double], // values 0.0 to 1.0, one for each bin
    color: Color = Color.blue): Chart = {
    val numBins = darkness.size
    val histData = HistogramDataset(data, numBins)
    val countsBinArray = histData.binArray
    Chart((countsBinArray zip darkness).zipWithIndex.map {
      case ((count, dark), binNumber) =>
        val dataset = new SingleHistogramBarDataset(count, binNumber, numBins, histData.rangeStart, histData.binWidth)
        val colorScale = 1 - dark.toFloat
        val Array(r, g, b) = color.getRGBColorComponents(null).map(v => v + ((1 - v) * colorScale))
        val newColor = new Color(r, g, b)
        SingleChart(dataset.dataset, BarRenderer(newColor))
    })
  }
}

object LineGraph {
  def make(
    data: Vector[(Double, Double)],
    color: Color = Color.blue,
    lineThickness: Int = 2,
    shape: Option[JShape] = None): Chart = {
    SingleChart(LineGraphDataset(data), LineRenderer(color = color, lineThickness = lineThickness))
  }

  def makeIndexed(
    data: Vector[Double],
    color: Color = Color.blue,
    lineThickness: Int = 2,
    shape: Option[JShape] = None): Chart = {
    make(data.zipWithIndex.map { case (y, x) => (x.toDouble, y) }, color, lineThickness)
  }
}

object ScatterGraph {
  def make(
    data: Vector[(Double, Double)],
    color: Color = Color.blue,
    shape: JShape = Shape.circle): Chart = {
    SingleChart(XYDataset(data), ScatterRenderer(color = color, shape = shape))
  }

  def makeIndexed(
    data: Vector[Double],
    color: Color = Color.blue,
    shape: JShape = Shape.circle): Chart = {
    make(data.zipWithIndex.map { case (y, x) => (x.toDouble, y) }, color, shape)
  }
}
