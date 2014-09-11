package chap4

object EitherFunctions {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Either[Exception, Double] =
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    a.foldRight[Either[E, List[B]]](Right(Nil))((next: A, acc) => {
      for {
        newValue <- f(next)
        accValue <- acc
      } yield newValue :: accValue
    })
  }
}
