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

/**
 * A chart for visualizing data
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
trait Chart {
  def charts: Vector[SingleChart]

  def draw(
    a: Int = 10, b: Int = 10,
    width: Int = 800, height: Int = 500,
    title: String = "",
    xaxisLabel: String = "",
    yaxisLabel: String = "",
    showLegend: Boolean = false) {

    val plot = new XYPlot()

    plot.setDomainAxis(0, new NumberAxis(xaxisLabel))
    plot.setRangeAxis(0, new NumberAxis(yaxisLabel))
    plot.setDatasetRenderingOrder(DatasetRenderingOrder.FORWARD)

    for ((SingleChart(dataset, renderer), i) <- charts.zipWithIndex) {
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
  dataset: XYSeriesCollection,
  renderer: XYItemRenderer)
  extends Chart {
  def charts = Vector(this)
}

case class MultiChart(
  primaryChart: Chart,
  additionalCharts: Vector[Chart])
  extends Chart {
  def charts = primaryChart.charts ++ additionalCharts.flatMap(_.charts)
}

object Chart {
  def apply(
    dataset: XYSeriesCollection,
    renderer: XYItemRenderer) = {
    SingleChart(dataset, renderer)
  }

  def make(
    primaryChart: Chart,
    additionalCharts: Chart*) = {
    MultiChart(primaryChart, additionalCharts.toVector)
  }
}

//
//
//

object BarChart {
  def make[A](data: Vector[(A, Double)]): Chart = {
    ???
  }

  def makeIndexed(
    data: Vector[Double]): Chart = {
    SingleChart(XYDataset(data.zipWithIndex.map { case (x, i) => (i.toDouble, x) }), BarRenderer())
  }
}

object Histogram {
  def make(
    data: Vector[Double],
    bins: Int): Chart = {
    SingleChart(HistogramDataset(data, bins), BarRenderer())
  }
}

object LineGraph {
  def make(
    data: Vector[(Double, Double)],
    dots: Boolean = true,
    lines: Boolean = true): Chart = {
    SingleChart(XYDataset(data), LineRenderer(lines = lines, shapes = dots))
  }

  def makeIndexed(
    data: Vector[Double],
    dots: Boolean = true,
    lines: Boolean = true): Chart = {
    make(data.zipWithIndex.map { case (y, x) => (x.toDouble, y) }, dots, lines)
  }
}
