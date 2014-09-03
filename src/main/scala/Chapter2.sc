def factorial(n: Int): Int = {
  def go(n: Int, acc: Int): Int = {
    if (n <= 0) acc
    else go(n - 1, n * acc)
  }

  go(n, 1)
}

def fib(n: Int): Int = {
  def go(current: Int, last: Int, secondLast: Int): Int = {
    print(s"$last ")
    if (current == n) last
    else go(current + 1, last + secondLast, last)
  }
  if (n == 0) 0
  else if (n == 1) 1
  else {
    go(1, 1, 0)
  }
}

def formatResult(name: String, n: Int, f: Int => Int): String = {
  val answer = f(n)
  s"The $name of $n is $answer"
}
//
//def isSorted[A](as: Array[A])

def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
  (b: B) => f(a, b)
}
val onePlusAnother = partial1(1, (a: Int, b: Int) => a + b)

onePlusAnother(5)

def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
  (a: A) => (b: B) => f(a, b)
}

def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
  (a: A, b: B) => f(a)(b)
}

def composeFunctions[A, B, C](g: A => B, f: B => C): A => C = {
  (a: A) => f(g(a))
}

val intToString : (Int => String) = (a : Int) => a.toString
val stringToDouble : (String => Double) = (s: String) => s.toDouble

val intToDouble = stringToDouble compose intToString

intToDouble(5)


val f = (x: Double) => math.Pi / 2 - x

f andThen math.sin








formatResult("factorial", 10, factorial)
formatResult("fib", 2, factorial)
formatResult("multiple", 4, (x: Int) => x * x)
formatResult("double", 4, x => x * x)
formatResult("half", 4, _ / 2)
factorial(2)
fib(0)
fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(6)
fib(7)
fib(8)
fib(9)