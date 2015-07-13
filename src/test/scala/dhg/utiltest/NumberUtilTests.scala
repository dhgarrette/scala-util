package dhg.utiltest

import scala.collection.mutable.Buffer

import org.junit.Assert._
import org.junit.Test

import dhg.util.TestUtil._
import dhg.util._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class NumberUtilTests {

  @Test
  def test_sum() {
    assertEquals("int", (5 ** 3).getClass.getName)
    assertEquals(125, 5 ** 3)
    assertEquals("double", (5 ** 3.0).getClass.getName)
    assertEquals(125.0, 5 ** 3.0, 0.000001)
    assertEquals("double", (5.0 ** 3).getClass.getName)
    assertEquals(125.0, 5.0 ** 3, 0.000001)
    assertEquals("double", (5.0 ** 3.0).getClass.getName)
    assertEquals(125.0, 5.0 ** 3.0, 0.000001)
  }

  @Test
  def test_Int_up() {
    assertEquals(4 to Int.MaxValue, 4.up)
  }

  @Test
  def test_Int_downto() {
    assertEquals(5 to 2 by -1, 5 downto 2)
  }

  @Test
  def test_dot() {
    assertEquals(3, Vector(1, 3, -5) dot Seq(4, -2, -1))
    assertEquals(3.0, Iterator(1.0, 3.0, -5) dot Seq(4.0, -2, -1.0), 1e-9)
  }

}
