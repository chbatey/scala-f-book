package chap10

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object MonoidMain {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = {
      a1 + a2
    }

    def zero(): Int = {
      0
    }
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = {
      a1 * a2
    }

    def zero: Int = {
      1
    }
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean]  {
    def op(a1: Boolean, a2: Boolean): Boolean = {
      a1 || a2
    }

    def zero: Boolean = {
      false
    }
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]  {
    def op(a1: Boolean, a2: Boolean): Boolean = {
      a1 && a2
    }

    def zero: Boolean = {
      true
    }
  }

  //ex-2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = {
      ???
    }

    def zero: Option[A] = {
      ???
    }
  }

  //ex-3
  def EndoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {

    override def op(a1: (A) => A, a2: (A) => A): (A) => A = ???

    override def zero: (A) => A = ???
  }

}



