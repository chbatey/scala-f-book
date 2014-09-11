package chap3


sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    def internalCount(tree: Tree[A], acc: Int): Int = {
      tree match {
        case Leaf(_) => acc + 1
        case Branch(left, right) => internalCount(left, acc) + internalCount(right, acc)
      }
    }
    internalCount(tree, 0)
  }

  def size2[A](tree: Tree[A]): Int = {
    fold(tree, 0)((a: A, b: Int) => b + 1)
  }

  def max(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => max(left) max max(right)
    }
  }

  def max2(tree: Tree[Int]): Int = {
    fold(tree, 0)((a,b) => a max b)
  }

  def maxDepth[A](tree: Tree[A]): Int = {
    def internalCount(tree: Tree[A], acc: Int): Int = {
      tree match {
        case Leaf(_) => acc
        case Branch(left, right) => internalCount(left, acc + 1) max internalCount(right, acc + 1)
      }
    }

    internalCount(tree, 0)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }
//
//  def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
//    fold(tree, )
//  }

  def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B): B = {
    tree match {
      case Leaf(a) => f(a, z)
      case Branch(left, right) => fold(right, fold(left, z)(f))(f)
    }
  }
}
