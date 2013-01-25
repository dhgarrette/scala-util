package dhg.util

import scala.collection.mutable.Buffer

import org.junit.Assert._
import org.junit.Test

import dhg.util.CollectionUtil._
import dhg.util.Collections._
import dhg.util.TestUtil._

class CollectionsTests {

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
