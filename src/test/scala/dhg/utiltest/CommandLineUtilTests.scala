package dhg.utiltest

import scala.collection.mutable.Buffer

import org.junit.Assert._
import org.junit.Test

import dhg.util.TestUtil._
import dhg.util._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class CommandLineUtilTests {

  @Test
  def test_parseArgs() {
    val (arguments, options) = parseArgs(Array(
      "--a1", "1a", 
      "--b2", "2", 
      "--c3", "3", 
      "--d4", "4.4", 
      "--e5a", "true", 
      "--e5b", "false"))

    assertEquals(Set("a1", "b2", "c3", "d4", "e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false"), options.getSeenEntries.toVector)

    assertEquals(true, options.contains("a1"))
    assertEquals(true, options.contains("e5a"))
    assertEquals(false, options.contains("f6"))
    assertEquals(false, options.contains("g7"))

    assertEquals(Set("a1", "b2", "c3", "d4", "e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false"), options.getSeenEntries.toVector)

    assertEquals("1a", options.s("a1"))
    assertEquals(Set("b2", "c3", "d4", "e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false"), options.getSeenEntries.toVector)

    assertExceptionMsg("--f6 not specified.*".r)(options.s("f6"))
    assertExceptionMsg("--f6 not specified.*".r)(options("f6"))

    assertEquals(Some(2), options.getI("b2"))
    assertEquals(Set("c3", "d4", "e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false"), options.getSeenEntries.toVector)

    assertEquals(None, options.getB("f6"))
    assertEquals(Set("c3", "d4", "e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false"), options.getSeenEntries.toVector)

    assertEquals(Some("2"), options.get("b2"))
    assertEquals(Set("c3", "d4", "e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false"), options.getSeenEntries.toVector)

    assertEquals(None, options.get("f6"))
    assertEquals(Set("c3", "d4", "e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false"), options.getSeenEntries.toVector)

    assertEquals(None, options.get("f6", _.toInt + 10))
    assertEquals(Set("c3", "d4", "e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false"), options.getSeenEntries.toVector)

    assertEquals(Some(12), options.get("b2", _.toInt + 10))
    assertEquals(Set("c3", "d4", "e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "12", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false"), options.getSeenEntries.toVector)

    assertEquals(2, options.i("b2"))
    assertEquals(Set("c3", "d4", "e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false"), options.getSeenEntries.toVector)

    assertEquals(3L, options.l("c3"))
    assertEquals(Set("d4", "e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false"), options.getSeenEntries.toVector)

    assertEquals(6, options.i("f6", 6))
    assertEquals(Set("d4", "e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false", "f6" -> "6"), options.getSeenEntries.toVector)

    assertEquals(true, options.contains("a1"))
    assertEquals(true, options.contains("e5a"))
    assertEquals(false, options.contains("f6"))
    assertEquals(false, options.contains("g7"))

    assertEquals(4.4, options.d("d4"), 0.000001)
    assertEquals(Set("e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false", "f6" -> "6"), options.getSeenEntries.toVector)

    assertEquals("2", options.s("b2"))
    assertEquals(Set("e5a", "e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false", "f6" -> "6"), options.getSeenEntries.toVector)

    assertEquals(true, options.b("e5a"))
    assertEquals(Set("e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false", "f6" -> "6"), options.getSeenEntries.toVector)

    assertEquals("6f", options.s("f6", "6f"))
    assertEquals(Set("e5b"), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false", "f6" -> "6f"), options.getSeenEntries.toVector)

    assertEquals(true, options.contains("a1"))
    assertEquals(true, options.contains("e5a"))
    assertEquals(false, options.contains("f6"))
    assertEquals(false, options.contains("g7"))

    assertEquals(false, options.b("e5b"))
    assertEquals(Set[String](), options.unusedOptions)
    assertEquals(Vector("a1" -> "1a", "b2" -> "2", "c3" -> "3", "d4" -> "4.4", "e5a" -> "true", "e5b" -> "false", "f6" -> "6f"), options.getSeenEntries.toVector)

    assertExceptionMsg("--f6 not specified.*".r)(options.s("f6"))

  }

}
