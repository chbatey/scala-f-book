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

  //ex5
  def setHead[A](l: List[A], newHead: A) = {
    l match {
      case Nil => Cons(newHead, Nil)
      case Cons(h, t) => Cons(newHead, t);
    }
  }

  //ex6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, rest) => Cons(h, init(rest))
    }
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    val reverse1: List[A] = List.reverse(l)
    foldRight(reverse1, z)((a: A, b:B) => f(b, a))
  }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def sumLeft(l: List[Int]) =
    foldLeft(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  def productRight(l: List[Int]) =
    foldLeft(l, 1.0)(_ * _)


  // ex-9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((a,b) => b + 1)
  }

  //ex-12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((acc, newValue) => Cons(newValue, acc))
  }

  //ex-14
  def append[A](l: List[A], value: A): List[A] = {
    foldRight(l, Cons(value, Nil))(Cons(_, _))
  }

  //ex-15
  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])( (newList, accList) =>
      foldRight(newList, accList)(Cons(_,_))
    )
  }

}

object Main extends App {
  val list: List[Int] = List(1, 2, 3, 4)
  val list2: List[Int] = List(5, 6, 7, 8)
  println("Original: " + list)

  println(List.tail(list))

  println(List.drop(list, 2))

  println(List.dropWhile(list)(_ != 4))

  println(List.setHead(list, 500))

  println(List.init(list))

  println(List.foldRight(list, Nil: List[Int])(Cons.apply))

  println("Length: " + List.length(list))
  println("Sum: " + List.sumLeft(list))
  println("Product: " + List.productRight(list))
  println("Reverse: " + List.reverse(list))
  println("Append: " + List.append(list, 5))
  println("Concat: " + List.concat(List(list, list2)))

}
