package dhg.util

import scala.collection.mutable.Buffer
import org.junit.Assert._
import org.junit.Test
import dhg.util.CollectionUtil._
import dhg.util.Collections._
import dhg.util.TestUtil._
import dhg.util.NumberUtil._

class NumberUtilTests {

  @Test
  def test_Int_up() {
    assertEquals(4 to Int.MaxValue, 4.up)
  }

}
