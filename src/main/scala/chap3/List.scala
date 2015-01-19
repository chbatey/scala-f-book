package chap3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](items: A*): List[A] = {
    if (items.isEmpty) Nil
    else Cons(items.head, apply(items.tail: _ *))
  }

  //ex2
  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(h, xs) => xs
    }
  }

  //ex3
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n - 1)
  }

  //ex4
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(a, tail) => {
        if (f(a)) dropWhile(tail)(f)
        else l
      }
    }
  }
}

object Main extends App {
  val list: List[Int] = List(1, 2, 3, 4)

  println(List.tail(list))

  println(List.drop(list, 2))

  println(List.dropWhile(list)(_ != 4))

}
