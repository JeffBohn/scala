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
}

