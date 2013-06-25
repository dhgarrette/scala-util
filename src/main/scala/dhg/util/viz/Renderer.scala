package dhg.util.viz

import java.awt.Color
import org.jfree.chart.renderer.xy.StandardXYBarPainter
import org.jfree.chart.renderer.xy.XYBarRenderer
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.BasicStroke

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
    shapes: Boolean = false) = {
    require(lineThickness > 0 || shapes)
    val r = new XYLineAndShapeRenderer(lineThickness > 0, shapes)
    r.setSeriesPaint(0, color)
    r.setSeriesStroke(0, new BasicStroke(lineThickness))
    r
  }
}

object ScatterRenderer {
  def apply(
    color: Color = Color.blue) = {
    LineRenderer(color, 0, true)
  }
}
