package dhg.util

import org.junit.Assert._
import org.junit.Test

import dhg.util.FileUtil._

/**
 * @author Dan Garrette (dhg@cs.utexas.edu)
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

}
