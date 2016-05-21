package dhg.utiltest

import org.junit.Assert._
import org.junit.Test

import dhg.util._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class FileUtilTests {

  @Test
  def test_pathjoin() {

    assertEquals("/a/b/c", pathjoin("/a/b", "c"))
    assertEquals("/a/b/c", pathjoin("/a/b/", "c"))
    assertEquals("/a/b/c", pathjoin("/a", "b/c"))
    assertEquals("/a/b/c", pathjoin("/a", "/b/c"))
    assertEquals("/a/b/c", pathjoin("/a/", "b/c"))
    assertEquals("/a/b/c", pathjoin("/a/", "/b/c"))

    assertEquals("a/b/c", pathjoin("a/b", "c"))
    assertEquals("a/b/c", pathjoin("a/b/", "c"))
    assertEquals("a/b/c", pathjoin("a", "b/c"))
    assertEquals("a/b/c", pathjoin("a", "/b/c"))
    assertEquals("a/b/c", pathjoin("a/", "b/c"))
    assertEquals("a/b/c", pathjoin("a/", "/b/c"))
  }
    
  @Test
  def test_relativeTo() {
    File("a/b/c/d").relativeTo(File("a/b")) //> res0: Option[java.io.File] = Some(c/d)
    File("a/b/c/d").relativeTo(File("a")) //> res1: Option[java.io.File] = Some(b/c/d)
    File("a/b/c/d").relativeTo(File("a/b/c/d")) //> res2: Option[java.io.File] = Some()
    File("a/b/c/d").relativeTo(File("/a/b")) //> res3: Option[java.io.File] = None
    File("a/b/c/d").relativeTo(File("")) //> res4: Option[java.io.File] = Some(a/b/c/d)
    File("a/b/c/d").relativeTo(File("/")) //> res5: Option[java.io.File] = Some(Applications/eclipse-3.7-2.10/Eclipse.app/
    //| Contents/MacOS/a/b/c/d)
    File("/a/b/c/d").relativeTo(File("")) //> res6: Option[java.io.File] = None
    File("/a/b/c/d").relativeTo(File("/a")) //> res7: Option[java.io.File] = Some(b/c/d)
  }
    
  @Test
  def test_parent() {
	  assertNull(File("a").getParent) //> res8: String = null
	  val p1: String = File("a/b").getParent; assertEquals("a", p1) //> res10: String = a
	  val p2: String = File("a/b/c").getParent; assertEquals("a/b", p2) //> res10: String = a/b

	  val p3: Option[File] = File("a").parent; assertTrue(p3.isEmpty) //> res9: Option[java.io.File] = None
    val p4: Option[File] = File("a/b").parent; assertEquals(Some(File("a")), p4) //> res11: Option[java.io.File] = Some(a)
    val p5: Option[File] = File("a/b/c").parent; assertEquals(Some(File("a/b")), p5) //> res11: Option[java.io.File] = Some(a/b)
  }
    
  @Test
  def test_name() {
    assertEquals("something.with.txt", File("/some/path/something.with.txt").name)
    assertEquals("something.txt", File("/some/path/something.txt").name)
    assertEquals("something", File("/some/path/something").name)
    assertEquals("something.with.txt", File("something.with.txt").name)
    assertEquals("something.txt", File("something.txt").name)
    assertEquals("something", File("something").name)
  }
    
  @Test
  def test_baseext() {
    assertEquals(("something.with", "txt"), File("/some/path/something.with.txt").baseext)
    assertEquals(("something", "txt"), File("/some/path/something.txt").baseext)
    assertEquals(("something", ""), File("/some/path/something").baseext)
    assertEquals(("something.with", "txt"), File("something.with.txt").baseext)
    assertEquals(("something", "txt"), File("something.txt").baseext)
    assertEquals(("something", ""), File("something").baseext)
  }
  
  @Test
  def test_basename() {
	  assertEquals("something.with", File("/some/path/something.with.txt").basename)
	  assertEquals("something", File("/some/path/something.txt").basename)
	  assertEquals("something", File("/some/path/something").basename)
	  assertEquals("something.with", File("something.with.txt").basename)
	  assertEquals("something", File("something.txt").basename)
	  assertEquals("something", File("something").basename)
  }
    
  @Test
  def test_ext() {
    assertEquals("txt", File("/some/path/something.with.txt").ext)
    assertEquals("txt", File("/some/path/something.txt").ext)
    assertEquals("", File("/some/path/something").ext)
    assertEquals("txt", File("something.with.txt").ext)
    assertEquals("txt", File("something.txt").ext)
    assertEquals("", File("something").ext)
  }

}
