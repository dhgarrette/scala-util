package dhg.util

import scala.collection.mutable.Buffer
import org.junit.Assert._
import org.junit.Test
import dhg.util.CollectionUtil._
import dhg.util.Collections._
import dhg.util.TestUtil._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class CollectionsTests {

  @Test
  def test_UniversalSet() {
    val u: Set[Int] = UniversalSet()
    assertTrue(u(5))
    assertTrue(u eq (u + 1))
    assertException(u.iterator) { case e: NotImplementedError => }
    assertException(u - 1) { case e: NotImplementedError => }
  }

  @Test
  def test_NextUntilIteratorish() {
    val i = new NextUntilIteratorish(Iterator[(Int, Symbol)](2 -> 'a, 3 -> 'b, 3 -> 'c, 5 -> 'd, 8 -> 'e, 9 -> 'f, 10 -> 'g))
    assertEquals(Vector(), i.nextUntil(_._1 <= 0))
    assertEquals(Vector(), i.nextUntil(_._1 <= 1))
    assertEquals(Vector(2 -> 'a), i.nextUntil(_._1 <= 2))
    assertEquals(Vector(3 -> 'b, 3 -> 'c), i.nextUntil(_._1 <= 3))
    assertEquals(Vector(), i.nextUntil(_._1 <= 4))
    assertEquals(Vector(5 -> 'd), i.nextUntil(_._1 <= 6))
    assertEquals(Vector(8 -> 'e, 9 -> 'f), i.nextUntil(_._1 <= 9))
    assertEquals(Vector(), i.nextUntil(_._1 <= 9))
    assertEquals(Vector(10 -> 'g), i.nextUntil(_._1 <= 12))
    assertEquals(Vector(), i.nextUntil(_._1 <= 14))
    assertEquals(Vector(), i.nextUntil(_._1 <= 16))
  }

  @Test
  def test_MemoMap() {

    val calculated = Buffer[String]()

    val m = new MemoMap(Map("a" -> 1, "b" -> 2, "c" -> 3),
      (key: String) => {
        calculated += key
        Map("d" -> 4, "e" -> 5, "f" -> 6)(key)
      })

    assertEqualsSameElements(Buffer(), calculated)
    assertEquals(1, m("a"))
    assertEqualsSameElements(Buffer(), calculated)
    assertEquals(4, m("d"))
    assertEqualsSameElements(Buffer("d"), calculated)
    assertEquals(5, m("e"))
    assertEqualsSameElements(Buffer("d", "e"), calculated)
    assertEquals(2, m("b"))
    assertEqualsSameElements(Buffer("d", "e"), calculated)

    assertEquals(1, m("a"))
    assertEqualsSameElements(Buffer("d", "e"), calculated)
    assertEquals(4, m("d"))
    assertEqualsSameElements(Buffer("d", "e"), calculated)
    assertEquals(5, m("e"))
    assertEqualsSameElements(Buffer("d", "e"), calculated)
    assertEquals(2, m("b"))
    assertEqualsSameElements(Buffer("d", "e"), calculated)
  }

}
