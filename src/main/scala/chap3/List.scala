package chap3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[T](ds: List[T]): List[T] = drop(1, ds)
  
  def drop[T](n: Int, ds: List[T]): List[T] = {
    if (n == 0) ds
    else ds match {
      case Nil => ds
      case Cons(_, t) => drop(n - 1, t)
    }
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(newHead, t)
  }

  // ex6

  def init[A](l: List[A]): List[A] = {
    def loop(acc: List[A], rest: List[A]): List[A] = {
      rest match {
        case Nil => acc
        case Cons(_, Nil) => acc
        case Cons(a, t) => loop(Cons(a, acc), t)
      }
    }
    loop(Nil, l)
  }

  // ex 9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  // ex 10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, t) =>
        foldLeft(t, f(z, x))(f)
    }
  }

  def length2[A](l: List[A]): Int = {
   foldLeft(l, 0)((acc, _) => acc + 1)
  }

  def sumLeft[A](l: List[A]): Int = {
    //foldLeft(l, 0)((acc, c) => c + acc)
    ???
  }

  // ex-12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((acc, a) => Cons(a, acc))
  }

  // ex-13
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): List[B] = {
    
    ???
  }

  // ex-14
  def append[A](l: List[A], item: A): List[A] = {
    foldRight(l, Cons(item, Nil))((a, acc) => Cons(a, acc))
  }

  // ex-15
  def concat[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)((a, acc) => Cons(a, acc))
  }

  // ex-15a
  def concatProper[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])((a, acc) => concat(a, acc))
  }

  // ex-16
  def mapOne(l: List[Int]): List[Int] = {
    l match {
      case Nil => Nil
      case Cons(x, t) => Cons(x + 1, mapOne(t))
    }
  }

  // ex-17
  def mapString(l: List[Double]): List[String] = {
    l match {
      case Nil => Nil
      case Cons(x, t) => Cons(x.toString, mapString(t))
    }
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(x, t) => Cons(f(x), map(t)(f))
    }
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    l match {
      case Nil => Nil
      case Cons(x, t) => concat(f(x), flatMap(t)(f))
    }     
  }

  // ex-21
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l) { a => if (f(a)) List(a) else Nil }
  }

  // ex-22
  def addList(l1: List[Int], l2: List[Int]): List[Int] = {

    ???
  }

    
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Example extends App {

  import List._

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3, 4)
  val example3 = List(1.0, 3.0, 0.0, 100.0)
  val total = sum(example)
  val lists2 = List(example, example2)

  println(total)
  println(tail(example))
  println(drop(2, example))
  println(dropWhile(example)(_ < 3))
  println(dropWhile(example)(_ < 10))
  println(setHead(example, 10))
  println(init(example))
  println(product2(example3))
  println("Fold right with Nil " + foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  println(length(example2))
  println(length2(example2))
  println(reverse(example2))
  println(append(example2, 10))
  println(concat(example, example2))
  println(concatProper(lists2))
  println(mapOne(example2))
  println(mapString(example3))
  println(flatMap(List(1,2,3))(i => List(i,i)))
  println(filter(List(1,2,3))(i => (i % 2) == 0))
}
