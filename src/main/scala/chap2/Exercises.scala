package chap2

import java.util

import scala.annotation.tailrec

object Exercises {
  // ex1
  def fib(n: Int): Int = {
    @tailrec
    def go(current: Int, last: Int, secondLast: Int): Int = {
      if (current == n) last + secondLast
      else go(current+1, last + secondLast, last)
    }
    if (n == 1) 0
    else if (n == 2) 1
    else go(3, 1, 0)
  }

  //ex2
  @tailrec
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    if (as.length <= 1) true
    else {
      gt(as(0), as(1)) && isSorted(as.drop(1), gt)
    }
  }

  //ex3
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a, b)
  }

  //ex4
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b:B) => f(a, b)
  }

  //ex5
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a:A, b: B) => f(a)(b)
  }

  //ex6
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a:A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(4))
    println(fib(5))
    println(fib(6))

    val t1 : Array[Int] = Array[Int](1,2,3,4,5, 5)
    println(s"${util.Arrays.toString(t1)} isSorted ${isSorted(t1, intGt)}")

    val t2 : Array[Int] = Array[Int](1)
    println(s"${util.Arrays.toString(t2)} isSorted ${isSorted(t2, (a:Int, b:Int) => a < b)}")

    val t3 : Array[Int] = Array[Int]()
    println(s"${util.Arrays.toString(t3)} isSorted ${isSorted(t3, (a:Int, b:Int) => a < b)}")

    val t4 : Array[Int] = Array[Int](1,5,4)
    println(s"${util.Arrays.toString(t4)} isSorted ${isSorted(t4, (a:Int, b:Int) => a < b)}")

    val partial: (Int) => Int = partial1(5, (a:Int, b:Int) => a * b)
    println(partial(10))
  }

  def intGt: (Int, Int) => Boolean = {
    (a: Int, b: Int) => {
//      println(s"$a, $b, ${a < b}")
      a <= b
    }
  }
}
