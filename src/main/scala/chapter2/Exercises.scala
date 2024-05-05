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
}