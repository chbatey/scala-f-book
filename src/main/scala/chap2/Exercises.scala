package chap2

object Exercises extends App {
  def fib(n: Int): Int = {
    def loop(i: Int, last: Int, current: Int): Int = {
      if (i == n) last + current
      else loop(i + 1, current, last + current)
    }
    loop(1, 0, 1)
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  def isSorted[A](as: Array[A])(gt: (A, A) => Boolean): Boolean = {
    for (i <- 1 until as.length) {
      if (!gt(as(i - 1), as(i))) return false
    }
    true
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b1) => f(a, b1)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a) => {
      (b) => f(a, b)
    }
  }

  def add (a:Int, b: Int): Int = a + b

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a) => f(g(a))
  }


  println(fib(1))
  println(fib(2))
  println(fib(3))
  println(fib(4))
  println(fib(5))

  println(isSorted(Array(1, 2, 3))((a, b) => a < b))
  println(isSorted(Array[Int]())((a, b) => a < b))
  println(isSorted(Array(1))((a, b) => a < b))
  println(isSorted(Array(2, 1))((a, b) => a < b))

  val partial = partial1(1, (a: Int, b: String) => s" b $b a  $a")
  println(partial("one"))

  val curriedAdd: (Int) => (Int) => Int = curry(add)
  val uncurriedAdd: (Int, Int) => Int = uncurry(curriedAdd)
  val partiallyAppliedAdd: (Int) => Int = curriedAdd(1)
  println(partiallyAppliedAdd(2))

}
