package chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => max(l) max max(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  private def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeWithFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maxWithFold(t: Tree[Int]): Int =
    fold(t)(x => x)(_ max _)

  def depthWithFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ max _)

//  def mapWithFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
//    fold(t)((v: A) => Leaf(f(v)) ) ((l: Tree[B], r: Tree[B]) => Branch(l,r): Tree[B])
}
