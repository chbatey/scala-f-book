package chap7

import java.util.concurrent._

object Par {

  val es = Executors.newFixedThreadPool(1)

  type Par[A] = ExecutorService => Future[A]

  def unit[T](c: T): Par[T] = es => {
    new Future[T]() {
      def cancel(force: Boolean) = false
      def get(time: Long, unit: TimeUnit): T = c
      def get(): T = c
      def isCancelled(): Boolean = false
      def isDone(): Boolean = true
    }
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def async[A](a: => A): Par[A] = fork(unit(a))
  def asyncF[A, B](f: A => B): A => Par[B] = a => { async(f(a)) }

  def map2[A, B, T](a: Par[A], b: Par[B])(f: (A, B) => T): Par[T] = {
    map(product(a, b))((pair) => f(pair._1, pair._2))
  }

  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] = {
    Par.unit((Par.run(es)(fa), Par.run(es)(fb)))
  }

  def map[A, B](fa: Par[A])(f: A => B): Par[B] = Par.unit(f(Par.run(es)(fa)))

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] = {
    Par.unit(l.foldLeft(List[A]()) { (acc, a) => Par.run(es)(a) :: acc })
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    l match {
      case Nil => Par.unit(Nil)
      case l if l.size == 1 => Par.unit(l.filter(f))
      case l => map2(parFilter(l.take(l.size / 2))(f), parFilter(l.drop(l.size / 2))(f))(_ ++ _)
    }
  }

  def run[A](s: ExecutorService)(a: Par[A]): A = a(s).get()

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    choiceN( map(a) {  should => { if (should) 0 else 1  } })(List(ifTrue, ifFalse))
  }

  def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = {
    map(a)(choice => Par.run(es)(choices(choice)))
  }

  def choiceMap[A,B](a: Par[A])(choices: Map[A,Par[B]]): Par[B] = {
    choices(Par.run(es)(a))
  }

  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = {
    f(Par.run(es)(a))
  }

  def join[A](a: Par[Par[A]]): Par[A] = Par.run(es)(a)
}

object Pars extends App {
  import Par._

  println("Pars")

  val one = unit({ println("one"); "one" })
  val two = unit({ println("two"); "two" })
  val combine = map2(one, two)((s1, s2) => s1 + s2)

  println(combine)
  println(run(es)(combine))

  println(run(es)(map(one)(_.toUpperCase())))

  val parF = parFilter(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) { _ % 2 == 0 }
  println(run(es)(parF))

  println(run(es)(choice(Par.unit(false))(Par.unit("true"), Par.unit("false"))))

  es.shutdownNow()
}

