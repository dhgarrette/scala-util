package dhg.util.viz

import java.awt.BorderLayout

import org.abego.treelayout.netbeans.AbegoTreeLayoutForNetbeans
import org.netbeans.api.visual.anchor.AnchorFactory
import org.netbeans.api.visual.graph.GraphScene
import org.netbeans.api.visual.layout.LayoutFactory
import org.netbeans.api.visual.widget.ConnectionWidget
import org.netbeans.api.visual.widget.LabelWidget
import org.netbeans.api.visual.widget.LayerWidget
import org.netbeans.api.visual.widget.Widget

import javax.swing.JDialog
import javax.swing.JScrollPane

trait VizTree {
  def label: String
  def children: Vector[VizTree]

  final def pretty: String = prettyLines.mkString("\n")
  private def prettyLines: Vector[String] = {
    children.flatMap(_.prettyLines) match {
      case Vector(childLine) => Vector(label + " " + childLine)
      case childLines => label +: childLines.map("  " + _)
    }
  }
}

object TreeViz {

  //case class NodeObj(label: String, index: Int)
  type NodeObj = String
  def NodeObj(label: String) = label

  private[this] class TreeScene(root: NodeObj) extends GraphScene[NodeObj, String] {

    private[this] val mainLayer = new LayerWidget(this)
    private[this] val connectionLayer = new LayerWidget(this)

    addChild(mainLayer)
    addChild(connectionLayer)

    def addEdge(a: NodeObj, b: NodeObj): Unit = {
      addEdge(a + "->" + b)
      setEdgeSource(a + "->" + b, a)
      setEdgeTarget(a + "->" + b, b)
    }

    def attachNodeWidget(n: NodeObj): Widget = {
      val w = new LabelWidget(this)
      w.setLabel(" " + n + " ")
      mainLayer.addChild(w)
      w
    }

    def attachEdgeWidget(e: String): Widget = {
      val connectionWidget = new ConnectionWidget(this)
      //connectionWidget.setTargetAnchorShape(AnchorShape.TRIANGLE_FILLED)
      connectionLayer.addChild(connectionWidget)
      connectionWidget
    }

    def attachEdgeSourceAnchor(edge: String, oldSourceNode: NodeObj, sourceNode: NodeObj): Unit = {
      val edgeWidget = findWidget(edge).asInstanceOf[ConnectionWidget]
      val sourceNodeWidget = findWidget(sourceNode)
      val sourceAnchor = AnchorFactory.createRectangularAnchor(sourceNodeWidget)
      edgeWidget.setSourceAnchor(sourceAnchor)
    }

    def attachEdgeTargetAnchor(edge: String, oldTargetNode: NodeObj, targetNode: NodeObj): Unit = {
      val edgeWidget = findWidget(edge).asInstanceOf[ConnectionWidget]
      val targetNodeWidget = findWidget(targetNode)
      val targetAnchor = AnchorFactory.createRectangularAnchor(targetNodeWidget)
      edgeWidget.setTargetAnchor(targetAnchor)
    }
  }

  private[this] def showScene(scene: TreeScene, title: String): Unit = {
    val panel = new JScrollPane(scene.createView())
    val dialog = new JDialog()
    dialog.setModal(true)
    dialog.setTitle(title)
    dialog.add(panel, BorderLayout.CENTER)
    dialog.setSize(800, 600)
    dialog.setVisible(true)
    dialog.dispose()
  }

  private[this] def layoutScene(scene: GraphScene[NodeObj, String], root: NodeObj): Unit = {
    val graphLayout = new AbegoTreeLayoutForNetbeans[NodeObj, String](root, 100, 100, 50, 50, true)
    val sceneLayout = LayoutFactory.createSceneGraphLayout(scene, graphLayout)
    sceneLayout.invokeLayoutImmediately()
  }

  private[this] class Counter(start: Int = 0) {
    private[this] var i = start
    def get = { val c = i; i += 1; c }
  }

  private[this] def createScene(t: VizTree): TreeScene = {
    val scene = new TreeScene(NodeObj(t.label))
    def addTree(t: VizTree): Unit = {
      scene.addNode(NodeObj(t.label))
      for (c <- t.children) {
        addTree(c)
        scene.addEdge(NodeObj(t.label), NodeObj(c.label))
      }
    }
    addTree(t)
    scene
  }

  def drawTree(t: VizTree): Unit = {
    val counter = new Counter
    def relabel(t: VizTree): (VizTree, (Int, Int)) = {
      if (t.children.isEmpty) {
        val i = counter.get
        (new VizTree { val label = f"${i}_${t.label}"; val children = Vector() }, (i, i))
      }
      else {
        val (newchildren, indexPairs) = t.children.map(relabel).unzip
        val (i, j) = (indexPairs.map(_._1).min, indexPairs.map(_._2).max)
        (new VizTree { val label = f"${i}-${j}_${t.label}"; val children = newchildren }, (i, j))
      }
    }

    val t2 = relabel(t)._1
    val s = createScene(t2)
    layoutScene(s, NodeObj(t2.label))
    showScene(s, "")
  }

}
