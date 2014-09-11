import chap4.Functions._
import chap4._

bothMatch(".*", ".*", "Chris")
bothMatch("", "", "Chris")

val isEven: (Int) => Option[Int] = number => if (number % 2 == 0) Some(number) else None


traverse[Int, Int](List(1,2,3))(isEven)
traverse[Int, Int](List(2,4,6))(isEven)



