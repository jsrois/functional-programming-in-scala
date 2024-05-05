package chapter2

import scala.annotation.tailrec

class Exercises {

  def fib(n: Int): Int = {
    @tailrec
    def go(a: Int, b: Int, c: Int): Int = {
      if (c <= 1) b
      else go(b, a+b, c-1)
    }
    go(0,1,n)
  }
}