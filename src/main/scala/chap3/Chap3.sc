import chap3._

val dropped = List.drop(List(1,2,3,4), 2)

val dropWhile = List.dropWhile[Int](List[Int](1,2,4,5,6), x => x < 3)

val oneToFour = List(1,2,3,4)

val replacedHeadNil = List.replaceHead(Nil, 5)

val replacedHead = List.replaceHead(oneToFour, 5)

List.foldRight(List(1,2,3,4), 1)((a,b) => a + b)
val partial = List.foldRight(List(1, 2, 3), Nil: List[Double]) _ //((a, b) => a + b)

val partialleft = List.foldLeft(List(1, 2, 3), Nil: List[Double]) _ //((a, b) => a + b)


partial((a,b) => Cons(a,b))
partialleft((a,b) => Cons(b,a))

List.foldRight(List(1,2,3,4), 0)((a,b) => b + 1)
List.foldRight2(List(1,2,3,4), 0)((a,b) => b + 1)
List.foldLeft(List(1,2,3,4), 0)((b,a) => b + 1)
List.foldLeft(List(1,2,3,4), Nil: List[Int])((a,b) => Cons(b, a))

List



