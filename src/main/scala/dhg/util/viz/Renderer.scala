package dhg.util.viz

import java.awt.Color
import org.jfree.chart.renderer.xy.StandardXYBarPainter
import org.jfree.chart.renderer.xy.XYBarRenderer
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.BasicStroke
import java.awt.{ Shape => JShape }
import org.jfree.util.ShapeUtilities
import java.awt.geom.Ellipse2D

/**
 * Factories for constructing renderers for charts
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */

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
    shape: Option[JShape] = None) = {
    require(lineThickness > 0 || shape.isDefined)
    val r = new XYLineAndShapeRenderer(lineThickness > 0, shape.isDefined)
    r.setSeriesPaint(0, color)
    r.setSeriesStroke(0, new BasicStroke(lineThickness))
    shape.foreach(s => r.setSeriesShape(0, s))
    r
  }
}

object ScatterRenderer {
  def apply(
    color: Color = Color.blue,
    shape: JShape = Shape.circle) = {
    LineRenderer(color, 0, Some(shape))
  }
}

object InvisibleXyRenderer {
  def apply() = new XYLineAndShapeRenderer(false, false)
}

//
//
//

object Shape {
  def circle: JShape = circle(5)
  def circle(size: Double) = new Ellipse2D.Double(-size / 2, -size / 2, size, size)
}
