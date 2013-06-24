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
import org.jfree.chart.renderer.category.BarRenderer
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

class SimpleChart(frame: JFrame) extends Chart {
  override def draw(a: Int, b: Int, width: Int, height: Int) {
    //frame.pack()
    frame.setBounds(a, b, width, height)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  }
}

//
//
//

trait ChartCustomizer[T <: Plot, Renderer <: AbstractRenderer] {
  final def customizeChart(chart: JFreeChart, chartTitle: String): Chart = {
    val backgroundColor = new Color(230, 230, 230)
    val foregroundAlpha = 1f // 0.7f

    chart.setBackgroundPaint(new Color(230, 230, 230))
    val plot = chart.getPlot.asInstanceOf[T]
    plot.setForegroundAlpha(foregroundAlpha)
    plot.setBackgroundPaint(Color.WHITE)
    // plot.setAxisOffset(new Spacer(Spacer.ABSOLUTE, 5.0, 5.0, 5.0, 5.0));

    setGridlinePaint(plot, new Color(150, 150, 150))
    setShadowVisible(plot, false)
    setSeriesPaint(plot, 0, Color.blue)
    useStandardPainter(plot)

    val x = new JFrame(chartTitle)
    x.setContentPane(new ChartPanel(chart))
    makeChart(x)

    //    val cpanel: ChartPanel = new ChartPanel(chart);
    //    getContentPane().add(cpanel, BorderLayout.CENTER);
  }

  def makeChart(frame: JFrame): Chart

  def setGridlinePaint(plot: T, color: Color)
  def getRenderer(plot: T): Renderer
  def setShadowVisible(plot: T, visible: Boolean)
  def setSeriesPaint(plot: T, series: Int, barColor: Color)
  def useStandardPainter(plot: T)
}

object BarChartChartCustomizer extends ChartCustomizer[CategoryPlot, BarRenderer] {
  override def makeChart(frame: JFrame) = new SimpleChart(frame)
  override def setGridlinePaint(plot: CategoryPlot, color: Color) {
    plot.setDomainGridlinePaint(color)
    plot.setRangeGridlinePaint(color)
  }
  override def getRenderer(plot: CategoryPlot): BarRenderer = plot.getRenderer.asInstanceOf[BarRenderer]
  override def setShadowVisible(plot: CategoryPlot, visible: Boolean) { getRenderer(plot).setShadowVisible(visible) }
  override def setSeriesPaint(plot: CategoryPlot, series: Int, barColor: Color) { getRenderer(plot).setSeriesPaint(series, barColor) }
  override def useStandardPainter(plot: CategoryPlot) { getRenderer(plot).setBarPainter(new StandardBarPainter()) }
}

object HistogramChartCustomizer extends ChartCustomizer[XYPlot, XYBarRenderer] {
  override def makeChart(frame: JFrame) = new SimpleChart(frame)
  override def setGridlinePaint(plot: XYPlot, color: Color) {
    plot.setDomainGridlinePaint(color)
    plot.setRangeGridlinePaint(color)
  }
  override def getRenderer(plot: XYPlot): XYBarRenderer = plot.getRenderer.asInstanceOf[XYBarRenderer]
  override def setShadowVisible(plot: XYPlot, visible: Boolean) { getRenderer(plot).setShadowVisible(visible) }
  override def setSeriesPaint(plot: XYPlot, series: Int, barColor: Color) { getRenderer(plot).setSeriesPaint(series, barColor) }
  override def useStandardPainter(plot: XYPlot) { getRenderer(plot).setBarPainter(new StandardXYBarPainter()) }
}

object LineGraphChartCustomizer extends ChartCustomizer[XYPlot, XYLineAndShapeRenderer] {
  override def makeChart(frame: JFrame) = new SimpleChart(frame)
  override def setGridlinePaint(plot: XYPlot, color: Color) {
    plot.setDomainGridlinePaint(color)
    plot.setRangeGridlinePaint(color)
  }
  override def getRenderer(plot: XYPlot): XYLineAndShapeRenderer = plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]
  override def setShadowVisible(plot: XYPlot, visible: Boolean) {}
  override def setSeriesPaint(plot: XYPlot, series: Int, barColor: Color) { getRenderer(plot).setSeriesPaint(series, barColor) }
  override def useStandardPainter(plot: XYPlot) {
    val renderer = getRenderer(plot)
    plot.setRenderer(renderer)
    // renderer.setBarPainter(new XYLineAndShapeRenderer())

    val rangeAxis: NumberAxis = plot.getRangeAxis().asInstanceOf[NumberAxis]
    rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
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
    yaxisLabel: String = "")(
      implicit a2Comparable: A => Comparable[_]): Chart = {

    BarChartChartCustomizer.customizeChart(
      ChartFactory.createBarChart(
        chartTitle,
        xaxisLabel,
        yaxisLabel,
        BarChartDataset(data.mapKeys(a2Comparable)), // data
        PlotOrientation.VERTICAL, // orientation
        false, // include legend
        false, // tooltips?
        false // URLs?
        ),
      chartTitle)
  }

  def makeIndexed(
    data: Vector[Double],
    chartTitle: String = "",
    xaxisLabel: String = "",
    yaxisLabel: String = ""): Chart = {
    make(data.zipWithIndex.map(_.swap), chartTitle, xaxisLabel, yaxisLabel)
  }
}

object Histogram {
  def make(
    data: Vector[Double],
    bins: Int,
    chartTitle: String = "",
    xaxisLabel: String = "",
    yaxisLabel: String = ""): Chart = {

    HistogramChartCustomizer.customizeChart(
      ChartFactory.createHistogram(
        chartTitle,
        xaxisLabel,
        yaxisLabel,
        HistogramDataset(data, bins), // data
        PlotOrientation.VERTICAL, // orientation
        false, // include legend
        false, // tooltips?
        false // URLs?
        ),
      chartTitle)
  }
}

object FakeHistogram {
  def make(
    data: Vector[Double],
    bins: Int,
    chartTitle: String = "",
    xaxisLabel: String = "",
    yaxisLabel: String = ""): FakeHistogram = {

    val dataset = FakeHistogramDataset(data, bins)
    val chart =
      BarChartChartCustomizer.customizeChart(
        ChartFactory.createBarChart(
          chartTitle,
          xaxisLabel,
          yaxisLabel,
          dataset, // data
          PlotOrientation.VERTICAL, // orientation
          false, // include legend
          false, // tooltips?
          false // URLs?
          ),
        chartTitle)
    new FakeHistogram(chart, dataset.binArray.max)
  }
}

class FakeHistogram(chart: Chart, val maxBarHeight: Int) extends Chart {
  override def draw(a: Int, b: Int, width: Int, height: Int) { chart.draw(a, b, width, height) }
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

    val chart = ChartFactory.createXYLineChart(
      chartTitle, // chart title
      xaxisLabel, // x axis label
      yaxisLabel, // y axis label
      XYDataset(data), // data
      PlotOrientation.VERTICAL,
      false, // include legend
      false, // tooltips
      false // urls
      )

    val renderer = LineGraphChartCustomizer.getRenderer(chart.getPlot.asInstanceOf[XYPlot])
    renderer.setSeriesShapesVisible(0, dots)
    renderer.setSeriesLinesVisible(0, lines)
    LineGraphChartCustomizer.customizeChart(chart, chartTitle)
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

object MultiChart {
  def make(
    primaryDataset: XYSeriesCollection,
    primaryRenderer: XYItemRenderer,
    chartData: Vector[(XYSeriesCollection, XYItemRenderer)] = Vector(),
    title: String = "",
    xaxisLabel: String = "",
    yaxisLabel: String = "",
    showLegend: Boolean = false) = {

    val plot = new XYPlot()

    plot.setDomainAxis(0, new NumberAxis(xaxisLabel))
    plot.setRangeAxis(0, new NumberAxis(yaxisLabel))

    for (((dataset, renderer), i) <- ((primaryDataset, primaryRenderer) +: chartData).zipWithIndex) {
      plot.setDataset(i, dataset)
      plot.setRenderer(i, renderer)
      plot.mapDatasetToDomainAxis(i, 0)
      plot.mapDatasetToRangeAxis(i, 0)
    }

    val chart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, showLegend)

    val frame = new JFrame(title)
    frame.setContentPane(new ChartPanel(chart))
//    frame.setBounds(10, 10, 800, 500)
//    frame.setVisible(true)
//    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    new SimpleChart(frame)
  }
}
