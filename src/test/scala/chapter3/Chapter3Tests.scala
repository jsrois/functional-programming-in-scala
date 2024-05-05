package chapter3

import chapter3.List.{setHead, sum, tail}
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

    assert(setHead(Nil,1) == List(1))
    assert(setHead(List(2,2,3,4,5),1) == List(1,2,3,4,5))
  }

}
