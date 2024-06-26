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

  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, l)
    case Cons(_, lt) => Cons(h, lt)
  }


  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, tail) => if (n == 0) l else drop(tail, n - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
  }


  def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
    as match {
      case Nil => acc
      case Cons(x, tail) => f(x, foldRight(tail, acc)(f))
    }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def productWithFoldLeft(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def lengthWithFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], acc: B)(f: (B, A) => B): B =
    as match {
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h))(f)
    }

  def add1toEach(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, add1toEach(t))
  }

  def eachToString(l: List[Int]): List[String] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, eachToString(t))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }

  def append[A](a: List[A], b: List[A]): List[A] = a match {
    case Nil => b
    case Cons(h, t) => Cons(h, append(t, b))
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(map(l)(f), Nil: List[B])(append)

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else List())

  def zipWithSum(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipWithSum(t1, t2))
  }

  def zipWith[A, B](a: List[A], b: List[A])(f: (A, A) => B): List[B] = (a, b) match {
    case (Nil, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }



}
