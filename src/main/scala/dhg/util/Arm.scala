package dhg.util

import java.io.File
import java.io.Reader
import java.io.Writer
import scala.io.Source
import java.io.Closeable

/**
 * Automatic Resource Management (ARM) utility.
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object Arm {

  trait Managed[T] {
    def self: T
    def close(): Unit
  }

  implicit class ManagedCloseable[T <: Closeable](val self: T) extends Managed[T] {
    def close() { self.close() }
  }

  implicit class ManagedSource[T <: Source](val self: T) extends Managed[T] {
    def close() { self.close() }
  }

  /**
   * Automatic Resource Management.  Ensure that the resource is closed after
   * executing the block.
   *
   * Example:
   *   using(new BufferedReader(new FileReader("file"))) { r =>
   *     var count = 0
   *     while (r.readLine != null) count += 1
   *     println(count)
   *   }
   */
  def using[T, R](resource: Managed[T])(block: T => R): R = {
    try {
      block(resource.self)
    }
    finally {
      resource.close()
    }
  }

  /**
   * Get an Iterator over the lines in the file.  The file will automatically
   * close itself when the end of the file is reached.  This gets around the
   * problem of having to all of your processing inside the `using` block.
   */
  def readLines(file: File, encoding: String = "UTF-8"): Iterator[String] = {
    val resource = Source.fromFile(file, encoding)
    val blockItr = resource.getLines
    var finished = false
    new Iterator[String] {
      override def next() = {
        hasNext()
        if (finished) throw new NoSuchElementException("next on empty iterator")
        val n = blockItr.next
        hasNext()
        n
      }
      override def hasNext() = {
        if (finished)
          false
        else {
          val hn = blockItr.hasNext
          if (!hn) {
            finished = true
            resource.close()
          }
          hn
        }
      }
    }
  }

}
