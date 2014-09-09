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
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(_, tail) => tail
  }

  def drop[A](list: List[A], n: Int): List[A] = {
    if (n <= 0) {
      list
    } else {
      list match {
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }
    }
  }

  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = {
    list match {
      case Nil => Nil
      case Cons(x, tail) if f(x) => dropWhile(tail, f)
      case _ => list
    }
  }

  def replaceHead[A](list: List[A], newHead: A): List[A] = {
    list match {
      case Nil => Cons(newHead, Nil)
      case Cons(currentHead, tail) => Cons(newHead, tail)
    }
  }

  // A is type of the list
  // B is the return type
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    ???
  }

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    val f2 = (right: B, left: A) => f(left, right)
    List.foldLeft(List.foldLeft(l, Nil: List[A])((a, b) => Cons(b, a)), z)(f2)
  }

  def sum2(l: List[Int]) = {
    foldRight(l, 0.0)(_ + _)
  }


  def product2(l: List[Double]) = {
    foldRight(l, 1.0)(_ * _)
  }

  def append[A](l: List[A], a: A): List[A] = {
    List.foldRight(l, Cons(a, Nil))((a, b) => Cons(a, b))
  }

  def listOfLists[A](listOfLists: List[List[A]]) = {
    val joinTwoLists: (List[A], List[A]) => List[A] = (firstList, secondList) => {
      foldRight(firstList, secondList)((newItem, accList) => Cons(newItem, accList))
    }
    foldLeft(listOfLists, Nil: List[A])(joinTwoLists)
  }

  def addOneToAll(list : List[Int]) : List[Int] = {
     foldRight(list, Nil: List[Int])((newItem, acc) => Cons(newItem + 1, acc))
  }

  def convertDoubleListToStringList(list: List[Double]) : List[String] = {
    foldRight(list, Nil: List[String])((newItem, acc) => Cons(newItem.toString, acc))
  }

  def map[A, B](list: List[A])(f: A => B) : List[B] = {
    foldRight(list, Nil: List[B])((newItem, acc) => Cons(f(newItem), acc))
  }

  def filter[A](list: List[A])(f: A => Boolean) : List[A] = {
    foldRight(list, Nil: List[A])((newItem, acc) => {
      if (f(newItem)) Cons(newItem, acc)
      else acc
    })
  }

  def flatMap[A, B](list: List[A])(f: A => List[B]) : List[B] = {
    foldRight(list, Nil: List[B])((newItem, acc) => listOfLists(List(f(newItem), acc)))
  }

  def filter2[A](list: List[A])(f: A => Boolean) : List[A] = {
    flatMap(list)((a) => {
      if (f(a)) List(a)
      else Nil
    })
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)
}
