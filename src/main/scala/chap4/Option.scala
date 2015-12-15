package chap4

import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this.flatMap(_ => ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) Some(a) else None
    case None => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Options extends App {
  println("Hello Option")

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // ex-2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map( x => math.pow(x - m, 2))))
  }

  // ex-3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap { a1: A  => b  map { b1: B  => f(a1,b1)  } }
  }

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  // ex-4
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    map2(pattern(pat1), pattern(pat2)) { (a, b) => a.matcher(s).matches && b.matcher(s).matches  }
    
  }

  // ex-5
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a) { a => a }
  }

  // ex-6
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(List()): Option[List[B]]) {
      (next, acc) => f(next).flatMap( n => acc.map( n :: _))
    }
  }

  val one = Some(1)
  val two = Some(2)
  println(map2(one, two) { (a, b) => a + b })
  println(bothMatch_2("hello.*", ".*world", "hello world"))
  println(bothMatch_2("hi.*", ".*world", "hello world"))
  println(sequence(List(Some(1), Some(2), Some(3))))
  println(sequence(List(Some(1), Some(2), Some(3), None)))  
}
