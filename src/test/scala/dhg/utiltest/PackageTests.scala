package dhg.utiltest

import org.junit.Assert._
import org.junit.Test

import dhg.util.TestUtil._
import scalaz._
import Scalaz._
import dhg.util._

class PackageTests {

  @Test
  def test_Validation_getOrElseThrow() {
    assertEquals("something", "something".success[Nothing].getOrElseThrow())
    assertException("something".failure[Nothing].getOrElseThrow()) { case e: RuntimeException => assertEquals("something", e.getMessage) }
    assertException(123.failure[Nothing].getOrElseThrow()) { case e: RuntimeException => assertEquals("123", e.getMessage) }
  }

}
