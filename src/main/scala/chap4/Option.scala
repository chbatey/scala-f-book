package chap4

trait Option[+A] {
  def map[B](f: A => B): chap4.Option[B]
  def flatMap[B](f: A => chap4.Option[B]): chap4.Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => chap4.Option[B]): chap4.Option[B]
  def filter(f: A => Boolean): chap4.Option[A]
}

case object None extends chap4.Option[Nothing] {
  def map[B](f: (Nothing) => B): Option[B] = None

  def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None

  def getOrElse[B >: Nothing](default: => B): B = default

  def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  def filter(f: (Nothing) => Boolean): Option[Nothing] = None
}

case class Some[+A](a: A) extends chap4.Option[A] {
  def map[B](f: (A) => B): Option[B] = Some(f(a))

  def flatMap[B](f: (A) => Option[B]): Option[B] = f(a)

  def getOrElse[B >: A](default: => B): B = a

  def orElse[B >: A](ob: => Option[B]): Option[B] = Some(a)

  def filter(f: (A) => Boolean): Option[A] = Some(a)
}


