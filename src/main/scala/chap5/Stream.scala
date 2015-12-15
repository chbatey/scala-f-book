package chap5

trait Stream[+A] {

  import Stream._

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeWhile(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]) { (a, b) => if (f(a)) cons(a, b) else b }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) => p(a) && b }

  def headOption(): Option[A] = {
    foldRight(None: Option[A]) { (a, b) => Some(a) }
  }

  def map[U](f: A => U): Stream[U] = {
    foldRight(Stream.empty[U]) { (a, b) => cons(f(a), b) }
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]) { (a, acc) => if (f(a)) cons(a, acc) else acc }
  }

  def append[U >: A](a: U): Stream[U] = {
    foldRight(Stream.cons(a, empty)) { (a, acc) => cons(a, acc) }
  }

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[U](f: (A => Stream[U])): Stream[U] = {
    foldRight(Stream.empty[U]) { (a, acc) => f(a) append acc }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)) { s =>
      s match {
        case (Cons(h, t), Cons(h2, t2)) => Some(((Some(h()), Some(h2())), (t(), t2())))
        case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
        case (Cons(h, t), Empty) => Some(((Some(h()), None)), (t(), empty))
        case (Empty, Empty) => None
      }
    }
  }

  // ex 5-14
  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).takeWhile { case (left, right)  => right.isDefined  }.forAll { case (left, right) => left == right }
  }

  // ex 5-15
  def tails: Stream[A] = {
    ???
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = {
    unfold(n - 1) { s => Some((s + 1, s + 1)) }
  }

  def fib(): Stream[Int] = {
    unfold((0, 1)) { case (i1, i2) => Some(i1 + i2, (i2, i1 + i2)) }
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  // ex 5-13

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

object Streams extends App {
  println("Streams")

  import Stream._

  println(cons(1, cons(2, empty)).toList)

  val five = (cons(1, cons(2, cons(3, cons(4, cons(5, empty))))))

  println(five.take(3).toList)
  println(Stream(1, 2, 3).take(2).toList)
  println(Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList)
  println(Stream().headOption())
  println(Stream(1, 2).headOption())
  println(Stream(1, 2, 3).map { a => println("mapping" + a); a * 2 })
  println(Stream(1, 2, 3).map { a => println("mapping" + a); a * 2 }.toList)
  println(Stream(1, 2, 3).filter { a => a % 2 == 0 }.toList)

  println(Stream(1, 2, 3).append(10))
  println(Stream(1, 2, 3).append(10).toList)

  println(fib().take(10).toList)

  println(Stream(1,2,3).zipAll(Stream("one", "two", "three", "four")).toList)
}

