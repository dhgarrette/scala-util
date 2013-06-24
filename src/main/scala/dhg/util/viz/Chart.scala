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

/**
 * A chart for visualizing data
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
trait Chart {
  def draw(
    a: Int = 10, b: Int = 10,
    width: Int = 800, height: Int = 500)
}

case class SimpleChart(
  title: String = "",
  xaxisLabel: String = "",
  yaxisLabel: String = "",
  showLegend: Boolean = false,
  primaryChart: (XYSeriesCollection, XYItemRenderer),
  additionalCharts: Vector[(XYSeriesCollection, XYItemRenderer)])
  extends Chart {

  def withTitle(title: String) = this.copy(title = title)
  def withXaxisLabel(xaxisLabel: String) = this.copy(xaxisLabel = xaxisLabel)
  def withYaxisLabel(yaxisLabel: String) = this.copy(yaxisLabel = yaxisLabel)
  def withShowLegend(showLegend: Boolean) = this.copy(showLegend = showLegend)

  override def draw(a: Int, b: Int, width: Int, height: Int) {
    val plot = new XYPlot()

    plot.setDomainAxis(0, new NumberAxis(xaxisLabel))
    plot.setRangeAxis(0, new NumberAxis(yaxisLabel))

    for (((dataset, renderer), i) <- (primaryChart +: additionalCharts).zipWithIndex) {
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

object Chart {
  def make(
    primaryChart: (XYSeriesCollection, XYItemRenderer),
    additionalCharts: (XYSeriesCollection, XYItemRenderer)*) = {
    new SimpleChart(primaryChart = primaryChart, additionalCharts = additionalCharts.toVector)
  }
}

//
//
//

object BarChart {
  def make[A](
    data: Vector[(A, Double)],
    chartTitle: String = "",
    xaxisLabel: String = "",
    yaxisLabel: String = ""): Chart = {
    ???
  }

  def makeIndexed(
    data: Vector[Double],
    chartTitle: String = "",
    xaxisLabel: String = "",
    yaxisLabel: String = ""): Chart = {
    Chart.make(
      (XYDataset(data.zipWithIndex.map { case (x, i) => (i.toDouble, x) }), BarRenderer()))
      .withTitle(chartTitle)
      .withXaxisLabel(xaxisLabel)
      .withYaxisLabel(yaxisLabel)
  }
}

object Histogram {
  def make(
    data: Vector[Double],
    bins: Int,
    chartTitle: String = "",
    xaxisLabel: String = "",
    yaxisLabel: String = ""): Chart = {

    Chart.make(
      (HistogramDataset(data, bins), BarRenderer()))
      .withTitle(chartTitle)
      .withXaxisLabel(xaxisLabel)
      .withYaxisLabel(yaxisLabel)
  }
}

object LineGraph {
  def make(
    data: Vector[(Double, Double)],
    dots: Boolean = true,
    lines: Boolean = true,
    chartTitle: String = "",
    xaxisLabel: String = "",
    yaxisLabel: String = ""): Chart = {
    require(lines || dots)

    Chart.make(
      (XYDataset(data), LineRenderer(lines = lines, shapes = dots)))
      .withTitle(chartTitle)
      .withXaxisLabel(xaxisLabel)
      .withYaxisLabel(yaxisLabel)
  }

  def makeIndexed(
    data: Vector[Double],
    dots: Boolean = true,
    lines: Boolean = true,
    chartTitle: String = "",
    xaxisLabel: String = "",
    yaxisLabel: String = ""): Chart = {
    make(data.zipWithIndex.map { case (y, x) => (x.toDouble, y) }, dots, lines, chartTitle, xaxisLabel, yaxisLabel)
  }
}
