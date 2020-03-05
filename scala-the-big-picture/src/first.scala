/**
 * Hello world program from Pluralsight course "Scala the big picture".
 * Plus 99 Scala problems from aperiodic.net/phil/scala/s-99/
 */
object first {
  def main(args: Array[String]) = {
    println("Hello, world")
    assert(10 == p01_FindLastElementOfList(List(1,2,3,4,5,6,7,8,9,10)), "P01")
    println("Goodbye")
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
  def loop() : Unit = for (i<-Range.inclusive(10,0,-1)) println(f"$i")

  /**
    * Exercise 5 from "Scala for the Impatient"
    * Write procedure countdown(n:Int) that prints numbers n to 0.
    */
  def countdown(n:Int) : Unit = for (i<-Range.inclusive(n,0,-1)) println(f"$i")

  /**
    * Exercise 6 from "Scala for the Impatient"
    * Write a for loop that computes the product of the unicode codes
    * of all letters in a string.  eg. "Hello" = 9415087488L.
    */
  def totaluni(s:String) : BigInt = {var t:BigInt=1;for (i <- s) t*=i;t}

  /**
    * Exercise 7 from "Scala for the Impatient"
    * (Without using a loop) Computes the product of the unicode codes
    * of all letters in a string.  eg. "Hello" = 9415087488L.
    * Exercise 8 satisfied by making this a function.
    */
  def product(s:String) : BigInt = {var t:BigInt=1;s.foreach(c=>t*=c);t}
  /**
    * Exercise 11 from "Scala for the Impatient"
    * date string interpolator
    */
  import java.time.LocalDate
  implicit class DateInterpolator(val sc: StringContext) extends AnyVal {
    def date(args: Any*): LocalDate = {
      assert(args.length==3,f"The date string interpolator requires exactly three arguments, not the ${args.length} provided")
      assert(sc.parts(0)=="",f"There must not be anything before the first argument, such as: ${sc.parts(0)}")
      assert(sc.parts(1)=="-",f"The first and second arguments must be separated by a dash, not: [${sc.parts(1)}]")
      assert(sc.parts(2)=="-",f"The second and third arguments must be separated by a dash, not: [${sc.parts(2)}]")
      assert(sc.parts(3)=="",f"There must not be anything after the third argument, such as: ${sc.parts(3)}")
      LocalDate.of(
        args(0).toString().toInt,
        args(1).toString().toInt,
        args(2).toString().toInt)
    }
  }


}


