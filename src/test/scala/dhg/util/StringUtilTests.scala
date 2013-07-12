package dhg.util

import scala.collection.mutable.Buffer
import org.junit.Assert._
import org.junit.Test
import dhg.util.CollectionUtil._
import dhg.util.Collections._
import dhg.util.TestUtil._
import dhg.util.StringUtil._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class StringUtilTests {

  @Test
  def test_rtrim() {
    assertEquals("", "".rtrim)
    assertEquals("this", "this".rtrim)
    assertEquals("this and that", "this and that".rtrim)
    assertEquals("this and that", "this and that ".rtrim)
    assertEquals("this and that", "this and that\t".rtrim)
    assertEquals("this and that", "this and that\n".rtrim)
    assertEquals("this and that", "this and that    \t \n  \t  ".rtrim)
  }

  @Test
  def test_splitlines() {
    assertEqualsArray(Array(""), "".splitlines)
    assertEqualsArray(Array(), "\n".splitlines)
    assertEqualsArray(Array("this and", "that"), "this and\nthat".splitlines)
    assertEqualsArray(Array("this and ", " that"), "this and \n that".splitlines)
  }

  @Test
  def test_splitWhitespace() {
    assertEqualsArray(Array(""), "".splitWhitespace)
    assertEqualsArray(Array(), " ".splitWhitespace)
    assertEqualsArray(Array("this", "and", "that"), "this and\nthat".splitWhitespace)
    assertEqualsArray(Array("this", "and", "that"), "this and \n that".splitWhitespace)
    assertEqualsArray(Array("this", "and", "that", "stuff"), "this   and \nthat\tstuff".splitWhitespace)
  }

  @Test
  def test_lsplit() {
    assertEquals("".split("x", 3).toVector, "".lsplit("x", 3))
    assertEquals("thisxthat".split("x", 3).toVector, "thisxthat".lsplit("x", 3))
    assertEquals("xxthisxxthatxxxstuffx".split("x").toVector, "xxthisxxthatxxxstuffx".lsplit("x"))
    assertEquals("xxthisxxthatxxxstuffx".split("x",100).toVector, "xxthisxxthatxxxstuffx".lsplit("x",100))
    assertEquals("xxthisxxthatxxxstuffxxxx".split("x").toVector, "xxthisxxthatxxxstuffxxxx".lsplit("x"))
    assertEquals("xxthisxxthatxxxstuffxxxx".split("x",100).toVector, "xxthisxxthatxxxstuffxxxx".lsplit("x",100))
    assertEquals("xxthisxxthatxxxstuffxxxx".split("x", 10).toVector, "xxthisxxthatxxxstuffxxxx".lsplit("x", 10))
    assertEquals("thisxandxxthatxandxstuff".split("x", 3).toVector, "thisxandxxthatxandxstuff".lsplit("x", 3))
    assertEquals("thisxandxxthatxandxxstuff".split("x", 3).toVector, "thisxandxxthatxandxxstuff".lsplit("x", 3))
    assertEquals("thisxandxxthatxandxxstuff".split("x+", 3).toVector, "thisxandxxthatxandxxstuff".lsplit("x+", 3))
    assertEquals("thisabcandabccthatabcandabcccstuff".split("abc+", 3).toVector, "thisabcandabccthatabcandabcccstuff".lsplit("abc+", 3))
  }

  @Test
  def test_rsplit() {
    assertEquals("".reverse.split("x", 3).map(_.reverse).reverse.toVector, "".rsplit("x", 3))
    assertEquals("thisxthat".reverse.split("x", 3).map(_.reverse).reverse.toVector, "thisxthat".rsplit("x", 3))
    assertEquals("xthisxxthatxxxstuffxx".reverse.split("x").map(_.reverse).reverse.toVector, "xthisxxthatxxxstuffxx".rsplit("x"))
    assertEquals("xthisxxthatxxxstuffxx".reverse.split("x", 100).map(_.reverse).reverse.toVector, "xthisxxthatxxxstuffxx".rsplit("x", 100))
    assertEquals("xxxxthisxxthatxxxstuffxx".reverse.split("x").map(_.reverse).reverse.toVector, "xxxxthisxxthatxxxstuffxx".rsplit("x"))
    assertEquals("xxxxthisxxthatxxxstuffxx".reverse.split("x", 100).map(_.reverse).reverse.toVector, "xxxxthisxxthatxxxstuffxx".rsplit("x", 100))
    assertEquals("xxxxthisxxthatxxxstuffxx".reverse.split("x", 10).map(_.reverse).reverse.toVector, "xxxxthisxxthatxxxstuffxx".rsplit("x", 10))
    assertEquals("thisxandxxthatxandxstuff".reverse.split("x", 3).map(_.reverse).reverse.toVector, "thisxandxxthatxandxstuff".rsplit("x", 3))
    assertEquals("thisxandxxthatxandxxstuff".reverse.split("x", 3).map(_.reverse).reverse.toVector, "thisxandxxthatxandxxstuff".rsplit("x", 3))
    assertEquals("thisxandxxthatxandxxstuff".reverse.split("x+", 3).map(_.reverse).reverse.toVector, "thisxandxxthatxandxxstuff".rsplit("x+", 3))
    assertEquals(Vector("thisabcandabccthat", "and", "stuff"), "thisabcandabccthatabcandabcccstuff".rsplit("abc+", 3))
  }

  @Test
  def test_wrap() {
    assertEquals("", "".wrap(10))
    assertEquals("this is a\ntest this\nis only a\ntest", "this is a test this is only a test".wrap(10))
    assertEquals("this is a\ntest\nthis is\nonly a\ntest", "this is a test\n this is only a test".wrap(10))
  }

  @Test
  def test_indent_spaces() {
    assertEquals("    ", "".indent(4))
    assertEquals("    this is a test this is only a test", "this is a test this is only a test".indent(4))
    assertEquals("    this is a test\n     this is only a\n    test", "this is a test\n this is only a\ntest".indent(4))
  }

  @Test
  def test_indent_string() {
    assertEquals(">>", "".indent(">>"))
    assertEquals(">>this is a test this is only a test", "this is a test this is only a test".indent(">>"))
    assertEquals(">>this is a test\n>> this is only a\n>>test", "this is a test\n this is only a\ntest".indent(">>"))
  }

}
