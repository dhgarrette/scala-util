package dhg.util.viz

import java.awt.Color
import org.jfree.chart.renderer.xy.StandardXYBarPainter
import org.jfree.chart.renderer.xy.XYBarRenderer
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer

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
    lines: Boolean = true,
    shapes: Boolean = false) = {
    val r = new XYLineAndShapeRenderer(lines, shapes)
    r.setSeriesPaint(0, color)
    r
  }
}

object ScatterRenderer {
  def apply(
    color: Color = Color.blue) = {
    LineRenderer(color, false, true)
  }
}
