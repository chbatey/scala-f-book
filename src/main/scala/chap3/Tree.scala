package chap3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Trees extends App {
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def max(tree: Tree[Int]): Int = {
    fold(tree, -1)(_ max _, _ max _)
  }

  def depth[A](tree: Tree[A]): Int = {
    fold(tree, 0)((a,b) => b + 1, (b, b2) => 1 + (b max b2))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  // ex-29
  def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B, f2: (B, B) => B): B = {
    tree match {
      case Leaf(a) => f(a, z)
      case Branch(left, right) => f2(fold(left, z)(f, f2), fold(right, z)(f, f2))
    }
  }

  val tree1 = Branch(Leaf(1), Branch(Leaf(5), Leaf(10)))
  val tree2 = Branch(Branch(tree1, Leaf(2)), Leaf(1))
  println("Size of leaf: " + size(Leaf(1)))
  println(size(Branch(Leaf(1), Leaf(2))))
  println(max(tree1))
  println("Depth: " + depth(tree1))
  println("Depth: " + depth(tree2))
  println("Map: " + tree2)
  println("Map: " + map(tree2)(_ * 2))
}

