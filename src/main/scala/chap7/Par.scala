package chap7

import java.util.concurrent._

trait Par[+A] {
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: => A): Par[A] = {
    exe : ExecutorService => {
      new FutureUnit[A](a)
    }
  }

  def fork[A](a: => Par[A]): Par[A] = {
    exe => {
      exe.submit(new Callable[A] {
        def call(): A = {
          a(exe).get()
        }
      })
    }
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = {
    exe => {
      new FutureUnit[C](f(pa(exe).get(), pb(exe).get()))
    }
  }

  def map[A,B](fa: Par[A])(f: A => B): Par[B] =
    map2(fa, unit(()))((a,_) => f(a))

  def sortPar(l: Par[List[Int]]) = map(l)(_.sorted)

  def async[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => fork(unit(f(a)))
  }

  def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence_simple(fbs)
  }

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    map[Boolean, A](a)(a => if (a) ifTrue else ifFalse)
  }

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))


  class FutureUnit[+A](a: => A) extends Future[A] {
    def cancel(mayInterruptIfRunning: Boolean): Boolean = true

    def isCancelled: Boolean = false

    def isDone: Boolean = true

    def get(): A = a

    def get(timeout: Long, unit: TimeUnit): A = a
  }

}
