package chap3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // ex 25
  def size[A](tree: Tree[A]) : Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  // ex 26
  def max[A](tree: Tree[A])(greater: (A, A) => A): A = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => greater(max(left)(greater), max(right)(greater))
    }
  }
}

object TreeMain extends App {

  val tree1 = Leaf(1)
  val tree2 = Branch(Leaf(1), Leaf(2))
  val tree3 = Branch(Leaf(4), Leaf(5))
  val tree4 = Branch(tree2, tree3)

  println(s"$tree1 size ${Tree.size(tree1)}")
  println(s"$tree2 size ${Tree.size(tree2)}")
  println(s"$tree4 max ${Tree.max(tree4)((a, b) => if (b > a) b else a)}")
}