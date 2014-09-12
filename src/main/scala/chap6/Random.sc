import chap6._

val rng = RNG.simple(1)
RNG.positiveInt(rng)

val d1 = RNG.double(rng)
val d2 = RNG.double(d1._2)
val d3 = RNG.double(d2._2)

val doubleInt = RNG.doubleInt(rng)

RNG.double3(rng)


RNG.ints(5)(rng)


RNG.positiveMax(500)(rng)

