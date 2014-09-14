package chap8

trait Prop {

  type SuccessCount = Int
  type FailedCase = String

  def check: Either[FailedCase, SuccessCount]

  def &&(o: Prop): Prop = {
    new Prop {
      def check: Either[String, SuccessCount] = {
        check.
      }
    }
  }
}
