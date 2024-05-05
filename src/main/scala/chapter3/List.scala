package chapter3

import scala.annotation.tailrec


sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](args: A*): List[A] =
    if (args.isEmpty) Nil
    else Cons(args.head, apply(args.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons (h, l)
    case Cons(_, lt) => Cons(h, lt)
  }


  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, tail) => if (n == 0) l else drop(tail, n-1)
    }
  }

}
