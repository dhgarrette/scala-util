package dhg.util

import scala.collection.mutable.SetBuilder

import org.junit.Assert._
import org.junit.Test

import dhg.util.CollectionUtil._
import dhg.util.TestUtil._

/**
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
class CollectionUtilTests {

  @Test
  def test_toTuple() {
    val seq = Seq(1, 2)
    val seqT: (Int, Int) = seq.toTuple2
    assertEquals((1, 2), seqT)

    assertException(seq.toTuple3) {
      case e: AssertionError => assertEquals("Cannot convert sequence of length 2 into Tuple3: List(1, 2)", e.getMessage)
    }

    val arr = Array("3", "4", "5", "6")
    val arrT: (String, String, String, String) = arr.toTuple4
    assertEquals(("3", "4", "5", "6"), arrT)

    assertException(arr.toTuple5) {
      case e: AssertionError => assertEquals("Cannot convert array of length 4 into Tuple5: Array(3, 4, 5, 6)", e.getMessage)
    }
  }

  @Test
  def test_prependAppendIterator() {
    val new1: Iterator[Int] = 1 +: Iterator(3, 4, 5)
    assertEqualsIterator(Iterator(1, 3, 4, 5), new1)

    val new2: Iterator[Int] = Iterator(3, 4, 5) :+ 7
    assertEqualsIterator(Iterator(3, 4, 5, 7), new2)

    val new3: Iterator[Int] = 1 +: 2 +: Iterator(3, 4, 5) :+ 6 :+ 7
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5, 6, 7), new3)
  }

  @Test
  def test_counts() {
    val coll1 = Vector('a, 'b, 'c, 'b, 'd, 'a, 'c, 'c, 'b)
    val grouped1: Map[Symbol, Int] = coll1.counts
    assertEquals(Map('a -> 2, 'b -> 3, 'c -> 3, 'd -> 1), grouped1)

    val coll2 = Iterator('a, 'b, 'c, 'b, 'd, 'a, 'c, 'c, 'b)
    val grouped2: Map[Symbol, Int] = coll2.counts
    assertEquals(Map('a -> 2, 'b -> 3, 'c -> 3, 'd -> 1), grouped2)
  }

  @Test
  def test_groupByKey() {
    val coll1 = Vector(1 -> 'a, 2 -> 'b, 3 -> 'c, 2 -> 'a, 2 -> 'b, 3 -> 'd)
    val grouped1: Map[Int, Vector[Symbol]] = coll1.groupByKey
    assertEquals(Map(1 -> Vector('a), 2 -> Vector('b, 'a, 'b), 3 -> Vector('c, 'd)), grouped1)

    val coll2 = Set(1 -> 'a, 2 -> 'b, 3 -> 'c, 2 -> 'a, 3 -> 'd)
    val grouped2: Map[Int, Set[Symbol]] = coll2.groupByKey
    assertEquals(Map(1 -> Set('a), 2 -> Set('b, 'a), 3 -> Set('c, 'd)), grouped2)
  }

  @Test
  def test_ungroup() {
    val grouped1 = Map(1 -> Vector('a), 2 -> Vector('b, 'a, 'b), 3 -> Vector('c, 'd))
    val coll1: Iterator[(Int, Symbol)] = grouped1.ungroup
    assertEqualsIterator(Iterator(1 -> 'a, 2 -> 'b, 2 -> 'a, 2 -> 'b, 3 -> 'c, 3 -> 'd), coll1)

    val grouped2 = Map(1 -> Set('a), 2 -> Set('b, 'a), 3 -> Set('c, 'd))
    val coll2: Iterator[(Int, Symbol)] = grouped2.ungroup
    assertEqualsIterator(Iterator(1 -> 'a, 2 -> 'b, 2 -> 'a, 3 -> 'c, 3 -> 'd), coll2)
  }

  @Test
  def test_dropRightWhile() {
    val coll1 = List(1, 2, 3, 4, 5)
    val res1: List[Int] = coll1.dropRightWhile(_ == 0)
    assertEquals(List(1, 2, 3, 4, 5), res1)

    val coll2 = Vector(1, 2, 3, 4, 5)
    val res2: Vector[Int] = coll2.dropRightWhile(_ > 3)
    assertEquals(Vector(1, 2, 3), res2)

    val coll3 = " this  and that "
    val res3: String = coll3.dropRightWhile(_ == ' ')
    assertEquals(" this  and that", res3)
  }

  @Test
  def test_splitAt() {
    val (first1: Iterator[Int], second1: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(3)
    assertEqualsIterator(Iterator(1, 2, 3), first1)
    assertEqualsIterator(Iterator(4, 5), second1)

    val (first2: Iterator[Int], second2: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(3)
    assertException(second2.next) {
      case e: AssertionError => assertEquals("assertion failed: first has NOT YET been read completely", e.getMessage)
    }
    assertEquals(1, first2.next)
    assertEquals(2, first2.next)
    assertEquals(3, first2.next)
    assertException(first2.next) {
      case e: AssertionError => assertEquals("assertion failed: first has already been read completely", e.getMessage)
    }
    assertEquals(4, second2.next)
    assertEquals(5, second2.next)
    assertException(second2.next) {
      case e: AssertionError => assertEquals("assertion failed: second has already been read completely", e.getMessage)
    }

    val (first3: Iterator[Int], second3: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(-1)
    assertEqualsIterator(Iterator(), first3)
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5), second3)

    val (first4: Iterator[Int], second4: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(0)
    assertEqualsIterator(Iterator(), first4)
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5), second4)

    val (first5: Iterator[Int], second5: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(5)
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5), first5)
    assertEqualsIterator(Iterator(), second5)

    val (first6: Iterator[Int], second6: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(6)
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5), first6)
    assertEqualsIterator(Iterator(), second6)
  }

  @Test
  def test_split() {
    // "12345".split('9')  -->  Array(12345)
    assertEqualsIterator(Iterator(Vector(1, 2, 3, 4, 5)), Vector(1, 2, 3, 4, 5).split(9))
    assertEqualsIterator(Iterator(Vector(1, 2, 3, 4, 5)), Iterator(1, 2, 3, 4, 5).split(9))

    // "12345".split('3')  -->  Array(12, 45)
    assertEqualsIterator(Iterator(List(1, 2), List(4, 5)), List(1, 2, 3, 4, 5).split(3))
    assertEqualsIterator(Iterator(Set(1, 2), Set(4, 5)), Iterator(1, 2, 3, 4, 5).split(3, new SetBuilder(Set[Int]())))

    // "1234225".split('2')  -->  Array(1, 34, "", 5)
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5)), Vector(1, 2, 3, 4, 2, 2, 5).split(2))
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5)), Iterator(1, 2, 3, 4, 2, 2, 5).split(2))

    // "212342225".split('2')  -->  Array("", 1, 34, "", "", 5)
    assertEqualsIterator(Iterator(Vector(), Vector(1), Vector(3, 4), Vector(), Vector(), Vector(5)), Vector(2, 1, 2, 3, 4, 2, 2, 2, 5).split(2))
    assertEqualsIterator(Iterator(Vector(), Vector(1), Vector(3, 4), Vector(), Vector(), Vector(5)), Iterator(2, 1, 2, 3, 4, 2, 2, 2, 5).split(2))

    // "221234225".split('2')  -->  Array("", "", 1, 34, "", 5)
    assertEqualsIterator(Iterator(Vector(), Vector(), Vector(1), Vector(3, 4), Vector(), Vector(5)), Vector(2, 2, 1, 2, 3, 4, 2, 2, 5).split(2))
    assertEqualsIterator(Iterator(Vector(), Vector(), Vector(1), Vector(3, 4), Vector(), Vector(5)), Iterator(2, 2, 1, 2, 3, 4, 2, 2, 5).split(2))

    // "12342252".split('2')  -->  Array(1, 34, "", 5)
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5)), Vector(1, 2, 3, 4, 2, 2, 5, 2).split(2))
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5)), Iterator(1, 2, 3, 4, 2, 2, 5, 2).split(2))

    // "123422522".split('2')  -->  Array(1, 34, "", 5)
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5)), Vector(1, 2, 3, 4, 2, 2, 5, 2, 2).split(2))
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5)), Iterator(1, 2, 3, 4, 2, 2, 5, 2, 2).split(2))

    // "1234225222".split('2')  -->  Array(1, 34, "", 5)
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5)), Vector(1, 2, 3, 4, 2, 2, 5, 2, 2, 2).split(2))
    assertEqualsIterator(Iterator(Vector(1), Vector(3, 4), Vector(), Vector(5)), Iterator(1, 2, 3, 4, 2, 2, 5, 2, 2, 2).split(2))
  }

  @Test
  def test_zipSafe() {
    val a = Vector(1, 2, 3)
    val b = List('a, 'b, 'c)

    val res1: Iterator[(Int, Symbol)] = a.iterator zipSafe b
    assertEqualsIterator(Iterator(1 -> 'a, 2 -> 'b, 3 -> 'c), res1)

    val res2: Vector[(Int, Symbol)] = a zipSafe b
    assertEquals(Vector(1 -> 'a, 2 -> 'b, 3 -> 'c), res2)

    val res3: Iterator[(Int, Symbol)] = (a.iterator, b).zipSafe
    assertEqualsIterator(Iterator(1 -> 'a, 2 -> 'b, 3 -> 'c), res3)

    val res4: List[(Int, Symbol)] = (a.toList, b).zipSafe
    assertEquals(List(1 -> 'a, 2 -> 'b, 3 -> 'c), res4)

    val c = Vector(1, 2, 3, 4)

    val res5: Iterator[(Symbol, Int)] = b.iterator zipSafe c
    assertEquals(('a, 1), res5.next)
    assertEquals(('b, 2), res5.next)
    assertEquals(('c, 3), res5.next)
    assertException(res5.next) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    val res6: Iterator[(Int, Symbol)] = c.iterator zipSafe b
    assertEquals((1, 'a), res6.next)
    assertEquals((2, 'b), res6.next)
    assertEquals((3, 'c), res6.next)
    assertException(res6.next) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    assertException(b zipSafe c) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    assertException(c zipSafe b) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    val res7: Iterator[(Symbol, Int)] = (b.iterator, c).zipSafe
    assertEquals(('a, 1), res7.next)
    assertEquals(('b, 2), res7.next)
    assertEquals(('c, 3), res7.next)
    assertException(res7.next) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    val res8: Iterator[(Int, Symbol)] = (c.iterator, b).zipSafe
    assertEquals((1, 'a), res8.next)
    assertEquals((2, 'b), res8.next)
    assertEquals((3, 'c), res8.next)
    assertException(res8.next) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    assertException((b, c).zipSafe) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    assertException((c, b).zipSafe) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }
  }

  @Test
  def test_unzip() {
    val itr1 = Iterator(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val (a1: Vector[Int], b1: Vector[Symbol]) = itr1.unzip
    assertEquals(Vector(1, 2, 3), a1)
    assertEquals(Vector('a, 'b, 'c), b1)

    import collection.mutable.{ ListBuffer, SetBuilder }
    val itr2 = Iterator(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val (a2: List[Int], b2: Set[Symbol]) = itr2.unzip(ListBuffer(), new SetBuilder(Set[Symbol]()))
    assertEquals(List(1, 2, 3), a2)
    assertEquals(Set('a, 'b, 'c), b2)
  }

  @Test
  def test_mapTo() {
    val coll1 = List(1, 2, 3, 4)
    val res1: List[(Int, Int)] = coll1.mapTo(_ + 2)
    assertEquals(List(1 -> 3, 2 -> 4, 3 -> 5, 4 -> 6), res1)

    val coll2 = Set(1, 2, 3, 4)
    val res2: Set[(Int, Int)] = coll2.mapTo(_ + 2)
    assertEquals(Set(1 -> 3, 2 -> 4, 3 -> 5, 4 -> 6), res2)

    val coll3 = Iterator(1, 2, 3, 4)
    val res3: Iterator[(Int, Int)] = coll3.mapTo(_ + 2)
    assertEqualsIterator(Iterator(1 -> 3, 2 -> 4, 3 -> 5, 4 -> 6), res3)
  }

  @Test
  def test_mapToVal() {
    var counter1 = 1
    val coll1 = List('a, 'b, 'c, 'd)
    val res1: List[(Symbol, Int)] = coll1.mapToVal({ counter1 *= 2; counter1 + 3 })
    assertEquals(List('a -> 5, 'b -> 7, 'c -> 11, 'd -> 19), res1)

    var counter2 = 1
    val coll2 = Set('a, 'b, 'c, 'd)
    val res2: Set[(Symbol, Int)] = coll2.mapToVal({ counter2 *= 2; counter2 + 3 })
    assertEquals(Set('a -> 5, 'b -> 7, 'c -> 11, 'd -> 19), res2)

    var counter3 = 1
    val coll3 = Iterator('a, 'b, 'c, 'd)
    val res3: Iterator[(Symbol, Int)] = coll3.mapToVal({ counter3 *= 2; counter3 + 3 })
    assertEqualsIterator(Iterator('a -> 5, 'b -> 7, 'c -> 11, 'd -> 19), res3)
  }

  @Test
  def test_mapKeys() {
    val coll1 = Map(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val res1: Map[Int, Symbol] = coll1.mapKeys(_ + 2)
    assertEquals(Map(3 -> 'a, 4 -> 'b, 5 -> 'c), res1)

    val coll2 = List(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val res2: List[(Int, Symbol)] = coll2.mapKeys(_ + 2)
    assertEquals(List(3 -> 'a, 4 -> 'b, 5 -> 'c), res2)

    var callCount = 0
    val coll3 = Map(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val res3: Map[Int, Symbol] = coll3.mapKeys(i => { callCount += 1; i + 2 })
    assertEquals(Map(3 -> 'a, 4 -> 'b, 5 -> 'c), res3)
    assertEquals(Map(3 -> 'a, 4 -> 'b, 5 -> 'c), res3)
    assertEquals(3, callCount)
  }

  @Test
  def test_mapVals() {
    val coll1 = Map('a -> 1, 'b -> 2, 'c -> 3)
    val res1: Map[Symbol, Int] = coll1.mapVals(_ + 2)
    assertEquals(Map('a -> 3, 'b -> 4, 'c -> 5), res1)

    val coll2 = List('a -> 1, 'b -> 2, 'c -> 3)
    val res2: List[(Symbol, Int)] = coll2.mapVals(_ + 2)
    assertEquals(List('a -> 3, 'b -> 4, 'c -> 5), res2)

    var callCount = 0
    val coll3 = Map('a -> 1, 'b -> 2, 'c -> 3)
    val res3: Map[Symbol, Int] = coll3.mapVals(i => { callCount += 1; i + 2 })
    assertEquals(Map('a -> 3, 'b -> 4, 'c -> 5), res3)
    assertEquals(Map('a -> 3, 'b -> 4, 'c -> 5), res3)
    assertEquals(3, callCount)
  }

  @Test
  def test_mapt() {
    val coll2_1 = Map(('a, 1), ('b, 2), ('c, 3))
    val res2_1: Map[String, Int] = coll2_1.mapt((x, y) => (x + "x", y + 2))
    assertEquals(Map(("'ax", 3), ("'bx", 4), ("'cx", 5)), res2_1)

    val coll2_2 = List(('a, 1), ('b, 2), ('c, 3))
    val res2_2: List[(String, Int)] = coll2_2.mapt((x, y) => (x + "x", y + 2))
    assertEquals(List(("'ax", 3), ("'bx", 4), ("'cx", 5)), res2_2)

    val coll2_3 = Iterator(('a, 1), ('b, 2), ('c, 3))
    val res2_3: Iterator[(String, Int)] = coll2_3.mapt((x, y) => (x + "x", y + 2))
    assertEqualsIterator(Iterator(("'ax", 3), ("'bx", 4), ("'cx", 5)), res2_3)

    val coll3_1 = Set(('a, 1, 5), ('b, 2, 6), ('c, 3, 7))
    val res3_1: Set[(String, Int, Int)] = coll3_1.mapt((x, y, z) => (x + "x", y + 2, z - 1))
    assertEquals(Set(("'ax", 3, 4), ("'bx", 4, 5), ("'cx", 5, 6)), res3_1)

    val coll3_2 = List(('a, 1, 5), ('b, 2, 6), ('c, 3, 7))
    val res3_2: List[(String, Int, Int)] = coll3_2.mapt((x, y, z) => (x + "x", y + 2, z - 1))
    assertEquals(List(("'ax", 3, 4), ("'bx", 4, 5), ("'cx", 5, 6)), res3_2)

    val coll3_3 = Iterator(('a, 1, 5), ('b, 2, 6), ('c, 3, 7))
    val res3_3: Iterator[(String, Int, Int)] = coll3_3.mapt((x, y, z) => (x + "x", y + 2, z - 1))
    assertEqualsIterator(Iterator(("'ax", 3, 4), ("'bx", 4, 5), ("'cx", 5, 6)), res3_3)
  }

  @Test
  def test_foldLeftWhile() {
    val col_7 = (1 to 5)
    val z_7 = List[Int]()
    def p_x_7(x: Int) = x < 3
    def op_7(z: List[Int], x: Int) = z :+ x
    val res_7: List[Int] = col_7.foldLeftWhile(z_7)((z, x) => p_x_7(x))(op_7)
    val exp_7 = List(1, 2)
    assertEquals(exp_7, col_7.takeWhile(p_x_7).foldLeft(z_7)(op_7))
    assertEquals(exp_7, res_7)

    val col_8 = (1 to 5)
    val z_8 = List[Int]()
    def p_z_8(z: List[Int]) = z.size < 3
    def op_8(z: List[Int], x: Int) = z :+ x
    val res_8: List[Int] = col_8.foldLeftWhile(z_8)((z, x) => p_z_8(z))(op_8)
    val exp_8 = List(1, 2, 3)
    assertEquals(exp_8, {
      var z = z_8
      val it = col_8.iterator
      while (p_z_8(z)) {
        val x = it.next
        z = op_8(z, x)
      }
      z
    })
    assertEquals(exp_8, res_8)

    val col_1 = (1 to 5)
    val res_1: Int = col_1.foldLeftWhile(0)((z, x) => z < 5)((z, x) => z + x)
    assertEquals(6, res_1)

    val col_2 = (1 to 5)
    val res_2: Int = col_2.foldLeftWhile(0)((z, x) => x < 3)((z, x) => z + x)
    assertEquals(3, res_2)

    val col_3 = (1 to 5)
    val res_3: Int = col_3.foldLeftWhile(0)((z, x) => z < 1)((z, x) => z + x)
    assertEquals(1, res_3)

    val col_4 = (1 to 5)
    val res_4: Int = col_4.foldLeftWhile(0)((z, x) => x < 1)((z, x) => z + x)
    assertEquals(0, res_4)

    val col_5 = (1 to 5)
    val res_5: Int = col_5.foldLeftWhile(0)((z, x) => z < 0)((z, x) => z + x)
    assertEquals(0, res_5)

    val col_6 = (1 to 5)
    val res_6: Int = col_6.foldLeftWhile(0)((z, x) => x < 0)((z, x) => z + x)
    assertEquals(0, res_6)
  }

  @Test
  def test_avg() {
    val coll1 = List(1, 2, 2, 5)
    val avg1: Double = coll1.avg
    assertEquals(2.5, avg1, 0.0000001)

    val coll2 = Set(1.0f, 1.5f, 2.5f, 5.0f)
    val avg2: Float = coll2.avg
    assertEquals(2.5f, avg2, 0.0000001)
  }

  @Test
  def test_normalize() {
    val coll1 = List(1, 2, 2, 5)
    val avg1: List[Double] = coll1.normalize
    assertEquals(List(0.1, 0.2, 0.2, 0.5), avg1)

    val coll2 = Set(1.0f, 1.5f, 2.5f, 5.0f)
    val avg2: Set[Float] = coll2.normalize
    assertEquals(Set(0.1f, 0.15f, 0.25f, 0.5f), avg2)
  }

  @Test
  def test_normalizeValues() {
    val coll1 = List('a -> 1, 'b -> 2, 'c -> 2, 'd -> 5)
    val avg1: List[(Symbol, Double)] = coll1.normalizeValues
    assertEquals(List('a -> 0.1, 'b -> 0.2, 'c -> 0.2, 'd -> 0.5), avg1)

    val coll2 = Set('a -> 1.0f, 'b -> 1.5f, 'c -> 2.5f, 'd -> 5.0f)
    val avg2: Set[(Symbol, Float)] = coll2.normalizeValues
    assertEquals(Set('a -> 0.1f, 'b -> 0.15f, 'c -> 0.25f, 'd -> 0.5f), avg2)

    val coll3 = Map('a -> 1.0, 'b -> 1.5, 'c -> 2.5, 'd -> 5.0)
    val avg3: Map[Symbol, Double] = coll3.normalizeValues
    assertEquals(Map('a -> 0.1, 'b -> 0.15, 'c -> 0.25, 'd -> 0.5), avg3)
  }

}
