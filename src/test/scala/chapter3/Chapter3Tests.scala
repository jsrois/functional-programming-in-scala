package chapter3

import chapter3.List.{add1toEach, drop, dropWhile, eachToString, foldLeft, length, lengthWithFoldLeft, productWithFoldLeft, setHead, sum, tail}
import org.scalatest.flatspec.AnyFlatSpec

class Chapter3Tests extends AnyFlatSpec {

  "exercise 3.1" should "find out what is the right match" in {

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    assert(x == 3)
  }

  "exercise 3.2" should "implement the tail function" in {
    assert(tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))
  }

  "exercise 3.3" should "set a different head for a list" in {
    assert(setHead(Nil, 1) == List(1))
    assert(setHead(List(2, 2, 3, 4, 5), 1) == List(1, 2, 3, 4, 5))
  }

  "exercise 3.4" should "drop N values from the head of the list" in {
    assert(drop(List(1, 2, 3, 4, 5), 2) == List(3, 4, 5))
    assert(drop(List(1, 2, 3, 4, 5), 6) == Nil)
    assert(drop(List(1, 2, 3, 4, 5), 1) == List(2, 3, 4, 5))
  }

  "exercise 3.5" should "drop while condition" in {
    assert(dropWhile(List(1, 2, 3, 4, 5))(_ < 4) == List(4, 5))
  }

  "exercise 3.9" should "calculate the length of a list using fold" in {
    assert(length(List(1, 2, 3, 4, 5)) == 5)
    assert(length(List(1, 2, 3)) == 3)
  }

  "exercise 3.10" should "implement fold left" in {
    assert(foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) == 15)
  }

  "exercise 3.11" should "implement sum, product and length using fold left" in {
    assert(productWithFoldLeft(List(1, 2, 3, 4)) == 24)
    assert(lengthWithFoldLeft(List(1, 2, 3, 4)) == 4)
  }

  "exercise 3.16" should "add 1 to each integer" in {
    assert(add1toEach(List(1, 2, 3)) == List(2, 3, 4))
  }

  "exercise 3.17" should "transform each element to string" in {
    assert(eachToString(List(1, 2, 3)) == List("1", "2", "3"))
  }

  "exercise 3.18" should "implement map" in {
    assert(List.map(List(1, 2, 3))(2 * _) == List(2, 4, 6))
  }

}
