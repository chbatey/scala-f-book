package chap8

case class Gen[A]()

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}

trait Prop {
  import Prop._
  def &&(prop: Prop): Prop = ???
  def check: Either[FailedCase, SuccessCount] = ???
}

object Gen {
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
}

object Gens extends App {
  println("Gens")


}
