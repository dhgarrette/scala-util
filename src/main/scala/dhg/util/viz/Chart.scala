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
import org.jfree.data.statistics.HistogramDataset
import javax.swing.JFrame
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.data.xy.XYDataset
import org.jfree.data.xy.XYSeries
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.CategoryPlot

class Chart(frame: JFrame) {
  def draw(
    a: Int = 10, b: Int = 10,
    width: Int = 800, height: Int = 500) {

    //frame.pack()
    frame.setBounds(a, b, width, height)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  }
}

object Chart {
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
    new Chart(x)

    //    val cpanel: ChartPanel = new ChartPanel(chart);
    //    getContentPane().add(cpanel, BorderLayout.CENTER);
  }

  def setGridlinePaint(plot: T, color: Color)
  def getRenderer(plot: T): Renderer
  def setShadowVisible(plot: T, visible: Boolean)
  def setSeriesPaint(plot: T, series: Int, barColor: Color)
  def useStandardPainter(plot: T)
}

object BarChartChartCustomizer extends ChartCustomizer[CategoryPlot, BarRenderer] {
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
  override def setGridlinePaint(plot: XYPlot, color: Color) {
    plot.setDomainGridlinePaint(color)
    plot.setRangeGridlinePaint(color)
  }
  override def getRenderer(plot: XYPlot): XYBarRenderer = plot.getRenderer.asInstanceOf[XYBarRenderer]
  override def setShadowVisible(plot: XYPlot, visible: Boolean) { getRenderer(plot).setShadowVisible(visible) }
  override def setSeriesPaint(plot: XYPlot, series: Int, barColor: Color) { getRenderer(plot).setSeriesPaint(series, barColor) }
  override def useStandardPainter(plot: XYPlot) { getRenderer(plot).setBarPainter(new StandardXYBarPainter()) }
}

object LingGraphChartCustomizer extends ChartCustomizer[XYPlot, XYLineAndShapeRenderer] {
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
    yaxisLabel: String = ""): Chart = {

    val dataset: DefaultCategoryDataset = new DefaultCategoryDataset()
    for ((key, value) <- data)
      dataset.addValue(value, "<none>", key.toString)

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

    val dataset = new HistogramDataset
    dataset.addSeries("", data.toArray, bins, 0, 1);

    HistogramChartCustomizer.customizeChart(
      ChartFactory.createHistogram(
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

    val series = new XYSeries(chartTitle)
    for ((x, y) <- data)
      series.add(x, y)
    val dataset = new XYSeriesCollection()
    dataset.addSeries(series)

    val chart = ChartFactory.createXYLineChart(
      chartTitle, // chart title
      xaxisLabel, // x axis label
      yaxisLabel, // y axis label
      dataset, // data
      PlotOrientation.VERTICAL,
      false, // include legend
      false, // tooltips
      false // urls
      )

    val renderer = LingGraphChartCustomizer.getRenderer(chart.getPlot.asInstanceOf[XYPlot])
    renderer.setSeriesShapesVisible(0, dots)
    renderer.setSeriesLinesVisible(0, lines)
    LingGraphChartCustomizer.customizeChart(chart, chartTitle)
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
