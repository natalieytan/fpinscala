package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // Exercise 3.26
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left).max(maximum(right))
    }
  }

  // Exercise 3.27
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + depth(left).max(depth(right))
    }
  }

  // Exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  // Exercise 3.29
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    tree match {
      case Leaf(value) => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }

  def sizeWithFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((s1, s2) => 1 + s1 + s2)

  def maximumWithFold(t: Tree[Int]): Int = fold(t)(value => value)((s1, s2) => s1.max(s2))

  def depthWithFold[A](t: Tree[A]): Int = fold(t)(_ => 0)((s1, s2) => 1 + s1.max(s2))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(value => Leaf(f(value)): Tree[B])(
      (s1, s2) => {
        Branch(s1, s2)
      }
    )
  }
}