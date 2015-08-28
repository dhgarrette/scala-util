package dhg.utiltest

import dhg.util.GraphUtil._
import org.junit.Test
import org.junit.Assert._
import dhg.util.TestUtil._

class GraphUtilTests {

  @Test
  def test_toDagOrder {
    @annotation.tailrec
    def assertDagOrder[A](orderSets: Vector[Set[A]], elements: Vector[A]): Unit = {
      //println(f"assertDagOrder: orderSets=$orderSets, elements=$elements")
      if (orderSets.isEmpty && elements.isEmpty) return
      assertTrue(f"elements=$elements", orderSets.nonEmpty)
      assertTrue(f"orderSets=$orderSets", elements.nonEmpty)
      val oSet +: remainingOrderSets = orderSets
      val e +: remainingElements = elements
      assertTrue(f"oSet=$oSet does not contain e=$e", oSet(e))
      assertDagOrder(if (oSet.size == 1) remainingOrderSets else (oSet - e) +: remainingOrderSets, remainingElements)
    }

    assertEquals(Vector[Int](), toDagOrder(Set[(Int, Int)]()))
    assertEquals(Vector(3, 4), toDagOrder(Set(3 -> 4)))
    assertEquals(Vector(3, 4, 5), toDagOrder(Set(3 -> 4, 4 -> 5, 3 -> 5)))
    assertDagOrder(Vector(Set(1, 2), Set(3), Set(4, 5)), toDagOrder(Set(1 -> 3, 2 -> 3, 3 -> 4, 3 -> 5)))
    assertDagOrder(Vector(Set(1), Set(2, 3), Set(4), Set(5)), toDagOrder(Set(1 -> 2, 1 -> 3, 2 -> 4, 3 -> 4, 4->5)))
    assertEquals(Vector(1, 2, 3, 4, 5), toDagOrder(Set(1 -> 2, 2 -> 3, 1 -> 3, 3 -> 4, 3 -> 5, 4 -> 5)))
    assertDagOrder(Vector(Set(1), Set(2), Set(3), Set(4, 5)), toDagOrder(Set(1 -> 2, 2 -> 3, 1 -> 3, 3 -> 4, 3 -> 5)))
    assertDagOrder(Vector(Set(1, 2), Set(3), Set(4), Set(5)), toDagOrder(Set(1 -> 3, 2 -> 3, 3 -> 4, 3 -> 5, 4 -> 5)))

    assertDagOrder(Vector(Set(1, 2, 3)), toDagOrder(Set[(Int, Int)](), Set(1, 2, 3)))
    assertDagOrder(Vector(Set(1, 2, 3), Set(4)), toDagOrder(Set(3 -> 4), Set(1, 2, 3)))
    assertDagOrder(Vector(Set(1, 2, 3), Set(4), Set(5)), toDagOrder(Set(3 -> 4, 4 -> 5, 3 -> 5), Set(1, 2, 3)))
    assertDagOrder(Vector(Set(6, 7, 1, 2), Set(3), Set(4, 5)), toDagOrder(Set(1 -> 3, 2 -> 3, 3 -> 4, 3 -> 5), Set(2, 4, 6, 7)))
    assertDagOrder(Vector(Set(6, 7, 1), Set(2, 3), Set(4), Set(5)), toDagOrder(Set(1 -> 2, 1 -> 3, 2 -> 4, 3 -> 4, 4->5), Set(2, 4, 6, 7)))
    assertDagOrder(Vector(Set(6, 7, 1), Set(2), Set(3), Set(4), Set(5)), toDagOrder(Set(1 -> 2, 2 -> 3, 1 -> 3, 3 -> 4, 3 -> 5, 4 -> 5), Set(2, 4, 6, 7)))
    assertDagOrder(Vector(Set(6, 7, 1, 2), Set(3), Set(4, 5)), toDagOrder(Set(1 -> 2, 2 -> 3, 1 -> 3, 3 -> 4, 3 -> 5), Set(2, 4, 6, 7)))
    assertDagOrder(Vector(Set(6, 7, 1, 2), Set(3), Set(4), Set(5)), toDagOrder(Set(1 -> 3, 2 -> 3, 3 -> 4, 3 -> 5, 4 -> 5), Set(2, 4, 6, 7)))

    assertExceptionMsg("assertion failed: Cannot compute dagOrder due to cycle!")(toDagOrder(Set(3 -> 3)))
    assertExceptionMsg("assertion failed: Cannot compute dagOrder due to cycle!")(toDagOrder(Set(3 -> 4, 4 -> 3)))
    assertExceptionMsg("assertion failed: Cannot compute dagOrder due to cycle!")(toDagOrder(Set(3 -> 4, 4 -> 3, 3 -> 5)))
    assertExceptionMsg("assertion failed: Cannot compute dagOrder due to cycle!")(toDagOrder(Set(3 -> 4, 4 -> 5, 5 -> 3)))
    assertExceptionMsg("assertion failed: Cannot compute dagOrder due to cycle!")(toDagOrder(Set(1 -> 3, 2 -> 3, 1 -> 3, 3 -> 4, 3 -> 5, 4 -> 2)))
    assertExceptionMsg("assertion failed: Cannot compute dagOrder due to cycle!")(toDagOrder(Set(1 -> 3, 2 -> 3, 1 -> 3, 3 -> 4, 3 -> 5, 4 -> 3)))
    assertExceptionMsg("assertion failed: Cannot compute dagOrder due to cycle!")(toDagOrder(Set(1 -> 3, 2 -> 3, 1 -> 3, 3 -> 4, 3 -> 5, 3 -> 3)))
  }

  @Test
  def test_hasCycles {
    assertFalse(hasCycles(Set[(Int, Int)]()))
    assertFalse(hasCycles(Set(3 -> 4)))
    assertTrue(hasCycles(Set(3 -> 3)))
    assertTrue(hasCycles(Set(3 -> 4, 4 -> 3)))
    assertTrue(hasCycles(Set(3 -> 4, 4 -> 3, 3 -> 5)))
    assertTrue(hasCycles(Set(3 -> 4, 4 -> 5, 5 -> 3)))
    assertFalse(hasCycles(Set(3 -> 4, 4 -> 5, 3 -> 5)))
    assertFalse(hasCycles(Set(1 -> 3, 2 -> 3, 1 -> 3, 3 -> 4, 3 -> 5)))
    assertTrue(hasCycles(Set(1 -> 3, 2 -> 3, 1 -> 3, 3 -> 4, 3 -> 5, 4 -> 2)))
    assertTrue(hasCycles(Set(1 -> 3, 2 -> 3, 1 -> 3, 3 -> 4, 3 -> 5, 4 -> 3)))
    assertTrue(hasCycles(Set(1 -> 3, 2 -> 3, 1 -> 3, 3 -> 4, 3 -> 5, 3 -> 3)))
    assertFalse(hasCycles(Set(1 -> 3, 2 -> 3, 1 -> 3, 3 -> 4, 3 -> 5, 4 -> 5)))
  }

}
