package chap6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val random: (Int, RNG) = rng.nextInt
    random match {
      case (Integer.MIN_VALUE, rng) => positiveInt(rng)
      case _ => (random._1.abs, random._2)
    }
  }


  def double: Rand[Double] = {
    map(positiveInt)(double => double / Integer.MAX_VALUE.toDouble)
  }


  def doubleInt: Rand[(Int, Double)] = {
    val di: (RNG) => ((Int, Double), RNG) = map2(positiveInt, double)((a, b) => (a, b))
    di
  }

  def double3: Rand[(Double, Double, Double)] = {
    val dd: (RNG) => ((Double, Double), RNG) = map2(double, double)((a, b) => (a, b))
    map2(dd, double)((c, d) => (c._1, c._2, d))
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (List(), rng)
    } else {
      val (int, rng2) = positiveInt(rng)
      val (list, nextRng) = ints(count - 1)(rng2)
      (int :: list, nextRng)
    }
  }

  def unit[A](a: A): Rand[A] = {
    rng => (a, rng)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def positiveMax(n: Int): Rand[Int] = {
    map(positiveInt)(int => int % n)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    ???
  }
}
