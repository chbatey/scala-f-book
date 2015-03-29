package chap10

import scala.language.higherKinds

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
      a1 orElse a2
    }

    def zero: Option[A] = {
      None
    }
  }

  //ex-3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {

    override def op(a1: (A) => A, a2: (A) => A): (A) => A = {
      (a: A) => a2(a1(a))
    }

    override def zero: (A) => A = {
      a => a
    }
  }

  // ex 5
  def wordsMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = {
      val trim1 = a1.trim
      val trim2 = a2.trim
      trim1 + " " + trim2
    }

    override def zero: String = ""
  }

  // ex 6
  def concatenate[A](as: List[A], m: Monoid[A]): A = {
    as.foldLeft(m.zero)(m.op)
  }

  //ex 7
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.map(f).foldLeft(m.zero)(m.op)
  }

  // ex 9
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = {
      (a1, a2) match {
        case (Stub(aa), Stub(bb)) => Stub(aa ++ bb)
        case (Part(l, count, r), Stub(bb)) => Part(l, count, r+bb)
        case (Stub(aa), Part(l, count, r)) => Part(aa+l, count, r)
        case (Part(l1, count1, r1), Part(l2, count2, r2)) => Part(l1, count1+count2 + (if ((r1 + l2).isEmpty) 0 else 1), r1)
      }
    }

    override def zero: WC = Stub("")
  }

  //ex 10 - TODO
  def wordCount(word: String): Int = {
    def recur(wc: WC) : Int = wc match {
      case Part("", n, "") => n
      case Stub("") => 0
      case Part(l, n, r) => recur(Stub(l)) + n + recur(Stub(r))
      case Stub(w) =>
        val at: (String, String) = w.splitAt(w.size / 2)
        recur(Part(at._1, 0, at._2))
    }
    recur(Stub(word))
  }

  //ex-11
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      v match {
        case IndexedSeq() => m.zero
        case IndexedSeq(a) => f(a)
        case _ =>
          val (first, second) = v.splitAt(v.length / 2)
//          println(s"$first $second")
          m.op(foldMapV(first, m)(f), foldMapV(second, m)(f))
      }
  }

  //ex-12
  val orderedMonoid = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = {
      if (a1 < a2)
        a2
      else
        a1
    }

    override def zero: Int = 0
  }

//  trait Foldable[F[_]] {
//    def foldRight[A, B](as: F[A])(f: (A, B) => B): B
//    def foldLeft[A, B](as: F[A])(f: (B, A) => B): B
//    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
//    def concatenate[A](as: F[A])(m: Monoid[A]): A =
//      as.foldLeft(m.zero)(m.op)
//  }

  //ex-17
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = {
    new Monoid[(A, B)] {

      override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

      override def zero: (A, B) = (A.zero, B.zero)
    }
  }

  
  def stringIntSomething: Monoid[(String, Int)] = productMonoid(stringMonoid, intAddition)


  def main(args: Array[String]): Unit = {
    val sentence = List("hello", "there", "i", " like ", "cassandra " )

    println("Fold left:")
    println(sentence.foldLeft(wordsMonoid.zero)(wordsMonoid.op))
    println("Fold right:")
    println(sentence.foldRight(wordsMonoid.zero)(wordsMonoid.op))

    println(concatenate(sentence, stringMonoid))

    println(wordCount(""))
//    println(wordCount("hello"))
//    println(wordCount("hello there"))
//    println(wordCount("hello there chris"))

    val vector = Vector(1,2,3,4,5)
    val multiplied = foldMapV(vector, intMultiplication)(a => a*2)
    println(multiplied)
//    println(foldMapV(vector, orderedMonoid)(a=>a) == vector.last)
    val v2: Vector[Int] = Vector(5, 4, 3, 2, 1)
    println(foldMapV(v2, orderedMonoid)(a=>a) == v2.last)
    val v3: Vector[Int] = Vector(1, 2, 3, 4, 3)
    println(foldMapV(v3, orderedMonoid)(a=>a) == v3.last)

    val v4: Vector[Int] = Vector(1)
    println(foldMapV(v4, orderedMonoid)(a=>a) == v4.last)

    val v5: Vector[Int] = Vector()
    println(foldMapV(v5, orderedMonoid)(a=>a))

    val pairs = List(("one", 1), ("two", 2))
    println(pairs.foldLeft(stringIntSomething.zero)(stringIntSomething.op))
  }
}



