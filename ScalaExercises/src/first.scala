import java.awt.datatransfer._
import java.time.LocalDate
import java.util.TimeZone

import scala.collection.mutable.{ArrayBuffer, Buffer}
import scala.reflect.ClassTag
import scala.util.Random
import scala.jdk.CollectionConverters._

/**
 * Hello world program from Pluralsight course "Scala the big picture".
 * Also exercises from Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
 * Also 99 Scala problems from aperiodic.net/phil/scala/s-99/
 */
object first {
  def main(args: Array[String]) = {
    println("Hello, world")
    assert(10 == p01_FindLastElementOfList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), "P01")
    assert(9415087488L == totaluni("Hello"))
    assert(9415087488L == product("Hello"))
    assert("1970-05-08" == {
      val y = 1970; val m = 5; val d = 8; date"$y-$m-$d";
    }.toString()) // Tests DateInterpolator
    for (c <- GenerateRandomArray(50)) {
      assert(c >= 0 && c < 50)
    }
    val a = Array(1, 2, 3, 4, 5)
    swapAdjacent(a)
    assert(a sameElements Array(2, 1, 4, 3, 5))
    val b = createWithSwapedAdjacent(a)
    assert(b sameElements Array(1, 2, 3, 4, 5))
    val c = positivityFirst(Array(1, -1, 2, -2, 0, -3, 3, 4, -5))
    assert(c sameElements Array(1, 2, 3, 4, -1, -2, 0, -3, -5))
    val d = average(Array(1.0, 2.0, 2.5, 3.0, 4.0))
    assert(d == 2.5)
    val e = Array(1, 2, 3, 4, 5)
    reverseSorted(e)
    assert(e sameElements Array(5, 4, 3, 2, 1))
    val f = ArrayBuffer(1, 2, 3, 4, 5)
    reverseSorted(f)
    assert(f sameElements ArrayBuffer(5, 4, 3, 2, 1))
    val g = removeDuplicates(Array(1, 2, 3, 2, 4, 5, 5, 5, 1, 1, 2, 2, 3, 4, 4, 5, 5, 5, 5))
    assert(g sameElements Array(1, 2, 3, 4, 5))
    val h = ArrayBuffer(1, -1, 2, -2, 3, 4, 5, -17)
    removeAllButFirstNegative(h)
    assert(h sameElements ArrayBuffer(1, -1, 2, 3, 4, 5))
    val i = ArrayBuffer(1, -1, 2, -2, 3, -3, 4, -4, 5, -5, -6, -7, -17, 6)
    improvedRemoveAllButFirstNegative(i)
    assert(i sameElements ArrayBuffer(1, -1, 2, 3, 4, 5, 6))
    val j = getAmericanTimeZones()
    assert(j.length == 166)
    assert(j(0)=="Adak")
    val k = obscureJavaListCall();
    assert(k(0)=="PNG")
    println("All tests successful, or an assertion would have failed above.")
  }

  def p01_FindLastElementOfList[T](list: List[T]): T = {
    //val count : Int = list.count(p=>true)
    //list.drop(count - 1).head
    list.last
  }

  /**
   * Exercise 4 from "Scala for the Impatient"
   * Equivalent of Java loop:
   * for (int i = 10; i >= 0; i--) System.out.println(i);
   */
  def loop(): Unit = for (i <- Range.inclusive(10, 0, -1)) println(f"$i")

  /**
   * Exercise 5 from "Scala for the Impatient"
   * Write procedure countdown(n:Int) that prints numbers n to 0.
   */
  def countdown(n: Int): Unit = for (i <- Range.inclusive(n, 0, -1)) println(f"$i")

  /**
   * Exercise 6 from "Scala for the Impatient"
   * Write a for loop that computes the product of the unicode codes
   * of all letters in a string.  eg. "Hello" = 9415087488L.
   */
  def totaluni(s: String): BigInt = {
    var t: BigInt = 1;
    for (i <- s) t *= i;
    t
  }

  /**
   * Exercise 7 from "Scala for the Impatient"
   * (Without using a loop) Computes the product of the unicode codes
   * of all letters in a string.  eg. "Hello" = 9415087488L.
   * Exercise 8 satisfied by making this a function.
   */
  def product(s: String): BigInt = {
    var t: BigInt = 1;
    s.foreach(c => t *= c);
    t
  }

  /**
   * Exercise 11 from "Scala for the Impatient"
   * date string interpolator
   */

  implicit class DateInterpolator(val sc: StringContext) extends AnyVal {
    def date(args: Any*): LocalDate = {
      assert(args.length == 3, f"The date string interpolator requires exactly three arguments, not the ${args.length} provided")
      assert(sc.parts(0) == "", f"There must not be anything before the first argument, such as: ${sc.parts(0)}")
      assert(sc.parts(1) == "-", f"The first and second arguments must be separated by a dash, not: [${sc.parts(1)}]")
      assert(sc.parts(2) == "-", f"The second and third arguments must be separated by a dash, not: [${sc.parts(2)}]")
      assert(sc.parts(3) == "", f"There must not be anything after the third argument, such as: ${sc.parts(3)}")
      LocalDate.of(
        args(0).toString().toInt,
        args(1).toString().toInt,
        args(2).toString().toInt)
    }
  }

  /**
   * Write a code snippet that sets a to an array of n random integers between 0 (inclusive) and n (exclusive).
   *
   * Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
   */
  def GenerateRandomArray(n: Int): Array[Int] = {
    val r = Random
    val a = new Array[Int](n)
    a.map(_ => r.nextInt(n))
  }

  def swapArrayElements[T](a: Array[T], i: Int, j: Int): Unit = {
    val t: T = a(j)
    a(j) = a(i)
    a(i) = t
  }

  /**
   * Write a loop that swaps adjacent elements of an array of integers. For example, Array(1, 2, 3, 4, 5) becomes Array(2, 1, 4, 3, 5).
   *
   * Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
   */
  def swapAdjacent[T](a: Array[T]): Unit = {
    for (i <- a.indices if i % 2 == 1) {
      swapArrayElements(a, i, i - 1)
    }
  }

  /**
   * Repeat the preceding assignment, but produce a new array with the swapped values. Use for/yield.
   * Array(1, 2, 3, 4, 5) becomes a new Array(2, 1, 4, 3, 5).
   *
   * Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
   */
  def createWithSwapedAdjacent[T: ClassTag](a: Array[T]): Array[T] = {
    val v: IndexedSeq[T] = for (i <- a.indices) yield
      if (i % 2 == 1)
        a(i - 1): T
      else if (i == a.length - 1)
        a(i): T
      else
        a(i + 1): T
    v.toArray
  }

  /**
   * Given an array of integers, produce a new array that contains all positive values of the original array, in their original order, followed by all values that are zero or negative, in their original order.
   *
   * Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
   */
  def positivityFirst(a: Array[Int]): Array[Int] = {
    (for (i <- a if (i > 0)) yield i) ++ (for (i <- a if (i < 1)) yield i)
  }

  /**
   * How do you compute the average of an Array[Double]?
   *
   * Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
   */
  def average(a: Array[Double]): Double = {
    val d: Double = a.length
    a.sum / d
  }

  /**
   * How do you rearrange the elements of an Array[Int] so that they appear in reverse sorted order?
   * How do you do the same with an ArrayBuffer[Int]?
   * (Part one)
   *
   * Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
   */
  def reverseSorted(a: Array[Int]) = {
    a.sortInPlaceWith((a, b) => a > b)
  }

  /**
   * How do you rearrange the elements of an Array[Int] so that they appear in reverse sorted order?
   * How do you do the same with an ArrayBuffer[Int]?
   * (Part two)
   * Takeaway:  Both classes work similarly.
   *
   * Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
   */
  def reverseSorted(a: ArrayBuffer[Int]) = {
    a.sortInPlaceWith((a, b) => a > b)
  }

  /**
   * Write a code snippet that produces all values from an array with duplicates removed. (Hint: Look at Scaladoc.)
   *
   * Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
   */
  def removeDuplicates[T](a: Array[T]): Array[T] = {
    a.distinct
  }

  /**
   * Suppose you are given an array buffer of integers and want to remove all but the first negative number.
   * Here is a sequential solution that sets a flag when the first negative number is called,
   * then removes all elements beyond.
   * Elided: Code sample
   * This is a complex and inefficient solution.
   * Rewrite it in Scala by collecting positions of the negative elements,
   * dropping the first element, reversing the sequence, and calling a.remove(i) for each index.
   *
   * Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
   */
  def removeAllButFirstNegative(a: ArrayBuffer[Int]) = {
    val positionsToRemove = for (i <- a.indices if a(i) < 0) yield i
    for (i <- positionsToRemove.tail.reverse) a.remove(i)
  }

  /**
   * Improve the solution of the preceding exercise by collecting the positions that should be moved and their target positions.
   * Make those moves and truncate the buffer.
   * Don’t copy any elements before the first unwanted element.
   *
   * (1, -1, 2, -2, 3, 4, 5, -17)
   *  0   1  2   3  4  5  6   7
   *             0            1
   *
   * Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
   */
  def improvedRemoveAllButFirstNegative(a: ArrayBuffer[Int]) = {
    val positionsToRemove = (for (i <- a.indices if a(i) < 0) yield i).tail
    val positionsToMove = a.indices.filter(i => i > positionsToRemove.head && positionsToRemove.contains(i) == false)
    val targetPositions = for(i<-positionsToMove.indices) yield positionsToMove(i) - positionsToRemove.count(l => l < positionsToMove(i))
    for(i<-targetPositions.indices) a(targetPositions(i)) = a(positionsToMove(i))
    a.trimEnd(positionsToRemove.length)
  }

  /**
   * Make a collection of all time zones returned by java.util.TimeZone.getAvailableIDs that are in America.
   * Strip off the "America/" prefix and sort the result.
   *
   * Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
   */
  def getAmericanTimeZones() : Array[String] = {
    val americaPrefix = "America/"
    TimeZone.getAvailableIDs().filter(_.startsWith(americaPrefix)).map(_.drop(americaPrefix.length));
  }

  /**
   * Import java.awt.datatransfer._ and make an object of type SystemFlavorMap with the call
   * val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]
   * Then call the getNativesForFlavor method with parameter DataFlavor.imageFlavor and get the return value as a Scala buffer.
   * (Why this obscure class? It’s hard to find uses of java.util.List in the standard Java library.)
    * Horstmann, Cay S. (2016-12-27T22:58:59). Scala for the Impatient . Pearson Education. Kindle Edition.
   */
  def obscureJavaListCall() = {
    val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]
    flavors.getNativesForFlavor(DataFlavor.imageFlavor).asScala; // Convert util.List[String] to a scala buffer
  }


}
