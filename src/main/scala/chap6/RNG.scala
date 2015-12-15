package chap6

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
    rng.nextInt match {
      case (Int.MinValue, _) => positiveInt(rng)
      case (n, rng) => (n.abs, rng)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    map(int) { _.toDouble / Int.MaxValue }(rng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, rng2) = positiveInt(rng)
    val (d, rng3) = double(rng2)

    ((int, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???
  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveMax(n: Int): Rand[Int] = map(_.nextInt) { _ % n }
}

object RNGs extends App {
  println("RNGs")

  import RNG._
  println(positiveInt(simple(1L)))
  println(double(simple(1L)))
  println(intDouble(simple(1L)))
  println(positiveMax(1000)(simple(1L)))
}

