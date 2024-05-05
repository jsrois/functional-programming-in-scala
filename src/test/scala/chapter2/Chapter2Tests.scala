package chapter2

import org.scalatest.flatspec.AnyFlatSpec

class Chapter2Tests extends AnyFlatSpec {

  val exercises = new Exercises()

  "exercise 2.1" should "return nth fibonaci number" in {

    assert(exercises.fib(1) == 1)
    assert(exercises.fib(2) == 1)
    assert(exercises.fib(3) == 2)
    assert(exercises.fib(4) == 3)
    assert(exercises.fib(5) == 5)
    assert(exercises.fib(6) == 8)

  }

  "exercise 2.2" should "return whether an array is sorted" in {
    assert(exercises.isSorted(Array(1, 2, 3, 4, 5),
      (a: Int, b: Int) => {a < b} ))

    assert(!exercises.isSorted(Array(1, 3, 2, 4, 5),
      (a: Int, b: Int) => {a < b} ))

    assert(!exercises.isSorted(Array(1, 2, 3, 4, 5),
      (a: Int, b: Int) => {a > b} ))

  }


}
