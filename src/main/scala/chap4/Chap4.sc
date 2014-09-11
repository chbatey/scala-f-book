import chap4.Functions._
import chap4._

bothMatch(".*", ".*", "Chris")
bothMatch("", "", "Chris")

val isEven: (Int) => Option[Int] = number => if (number % 2 == 0) Some(number) else None


traverse[Int, Int](List(1,2,3))(isEven)
traverse[Int, Int](List(2,4,6))(isEven)

for {
  age <- Right(42)
  name <- Left("invalid name")
  salary <- Right(1000000.0)
} yield name

for {
  age <- Right(42)
  name <- Right("Chris")
  salary <- Right(1000000.0)
} yield name

val isEvenEither: (Int) => Either[String, Int] = number => if (number % 2 == 0) Right(number) else Left("Not even")

EitherFunctions.traverse(List(1,2,3))(isEvenEither)
EitherFunctions.traverse(List(2,4,6))(isEvenEither)

EitherFunctions.traverse(List(1,2,3))(isEvenEither).orElse(Right(10))



