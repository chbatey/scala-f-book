package chap5

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = {
    def go(stream: Option[(A, Stream[A])], acc: List[A]) : List[A] = {
      stream match {
        case None => acc
        case Some((value, tail)) => go(tail.uncons, value :: acc)
      }
    }
    go(uncons, Nil).reverse
  }

  def take(n: Int) : Stream[A] = {
    def go(n: Int, next: Option[(A, Stream[A])], acc: Stream[A]) : Stream[A] = {
      if (n <= 0) acc
      next match {
        case Some((head, tail)) => go(n-1, tail.uncons, Stream.cons(head, acc))
        case None => acc
      }
    }

    go(n, this.uncons, Stream.empty)
  }

}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}
