package chap4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}
case class Left[+E](value: E) extends Either[E, Nothing] {
  def map[B](f: Nothing => B): Either[E, B] = this
  def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this
  def orElse[EE >: E,B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this.flatMap( a => b.map( b => f(a,b)))
  
}
case class Right[+A](value: A) extends Either[Nothing, A] {
  def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
  def flatMap[Nothing, B](f: A => Either[Nothing, B]): Either[Nothing, B] = f(value)
  def orElse[Nothing,B >: A](b: => Either[Nothing, B]): Either[Nothing, B] = this
  def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b.map(f(value, _))
}

object Eithers extends App {
  println("Eithers")

  // ex-8
  def traverse[A, B, C](a: List[A])(f: A => Either[C, B]): Either[C, List[B]] = {
    a.foldRight(Right(List[B]()): Either[C, List[B]]) {
      (next, acc) =>  f(next).flatMap { newB => acc.map { newB :: _ } }
    }
  }

  def sequence[A, B](list: List[Either[A, B]]): Either[A, List[B]] = {
    traverse(list) { e => e }
  }

  val employee = for {
    age <- Right(42)
    name <- Left("invalid name")
    salary <- Right(1000000)
  } yield (age, name, salary)

  println(employee)
  println(sequence(List(Right("one"), Right("two"), Right("three"))))
  println(sequence(List(Right("one"), Left("haha"), Right("two"), Right("three"))))
}
