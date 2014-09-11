package chap4

object Functions {

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((newValue: A, acc: Option[List[B]]) => map2(f(newValue), acc)(_ :: _))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case head :: tail => head flatMap (hh => sequence(tail) map (hh :: _))
    }
  }

  def sequence_2[A](a: List[Option[A]]): Option[List[A]] = {
      a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aValue <- a
      bValue <- b
    } yield f(aValue, bValue)
  }

  def map3[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap( (aValue: A) => b.map ( (bValue: B) => f(aValue, bValue) ))
  }

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher(pat), mkMatcher(pat2))((patMatch1, patMatch2) => patMatch1(s) && patMatch2(s))
  }

  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  import java.util.regex._

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }
}
