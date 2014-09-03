import chap3._

val dropped = List.drop(List(1,2,3,4), 2)

val dropWhile = List.dropWhile[Int](List[Int](1,2,4,5,6), x => x < 3)

val oneToFour = List(1,2,3,4)

val replacedHeadNil = List.replaceHead(Nil, 5)

val replacedHead = List.replaceHead(oneToFour, 5)

