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
      (a: Int, b: Int) => {
        a < b
      }))

    assert(!exercises.isSorted(Array(1, 3, 2, 4, 5),
      (a: Int, b: Int) => {
        a < b
      }))

    assert(!exercises.isSorted(Array(1, 2, 3, 4, 5),
      (a: Int, b: Int) => {
        a > b
      }))

  }

  "exercise 2.3" should "implement a currying function" in {
    val f = (name: String, age: Int) => "My name is %s and I'm %s years old".formatted(name, age)

    assert(exercises.curry(f)("Javi")(30) == f("Javi", 30))
  }

  "exercise 2.4" should "implement an uncurry function" in {
    val f = (name: String) => (age: Int) => "My name is %s and I'm %s years old".formatted(name, age)

    assert(exercises.uncurry(f)("Javi", 30) == f("Javi")(30))
  }

  "exercise 2.5" should "compose two functions" in {
    val f = (n: Int) => n.toString
    val g = (name: String) => name.length

    val result = exercises.compose(f,g)("Javi")

    assert(result == "4")
  }
}
