package chapter2

import scala.annotation.tailrec

class Exercises {

  def fib(n: Int): Int = {
    @tailrec
    def go(a: Int, b: Int, c: Int): Int = {
      if (c <= 1) b
      else go(b, a + b, c - 1)
    }

    go(0, 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean = {
      if (i >= as.length) true
      else ordered(as(i - 1), as(i)) && loop(i + 1)
    }

    loop(1)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}