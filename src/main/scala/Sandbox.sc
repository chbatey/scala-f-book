case class Player(name: String, score: Long)

def winner(p1: Player, p2: Player): Player = {
  if (p1.score > p2.score) p1
  else p2
}

val players = List(Player("Chris", 1), Player("Yo", 2), Player("James", 3))

players.reduceLeft(winner)
players.reduceRight(winner)



