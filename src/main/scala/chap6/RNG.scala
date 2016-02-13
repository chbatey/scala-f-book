package chap6

case class State[S,+A](run: S => (A,S)) {

  def map[B](f: A => B): State[S, B] = {
    flatMap { a => State.unit(f(a)) }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State( state => {
      val (a, s) = run(state)
      f(a).run(s)
    })
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
        flatMap(a => sb.map(b => f(a, b)))

}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      (
        (seed2 >>> 16).asInstanceOf[Int],
        simple(seed2)
      )
    }
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    State[RNG, Int](_.nextInt).flatMap { int => int match {
      case Int.MinValue => State(positiveInt)
      case _ => State.unit(int.abs)
    }}.run(rng)

  }

  def double(rng: RNG): (Double, RNG) = {
    State[RNG, Int](_.nextInt).map { i => i.toDouble / Int.MaxValue }.run(rng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???
  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???
 
  type Rand[+A] = State[RNG, Int]

  def positiveMax(n: Int): Rand[Int] = State[RNG, Int](_.nextInt).map { _ % n }
}

object RNGs extends App {
  println("RNGs")

  import RNG._
  println(positiveInt(simple(1L)))
  println(double(simple(1L)))
  println(positiveMax(1000).run(simple(1L)))

  val int = State[RNG, Int](_.nextInt)

  val blah = for {
    i <- int
    j <- int
  } yield (i, j)

  println(blah.run(simple(1L)))
  

}

