package chapter3

import chapter3.List.{add1toEach, append, drop, dropWhile, eachToString, filterWithFlatMap, foldLeft, length, lengthWithFoldLeft, productWithFoldLeft, setHead, sum, tail, zipWith, zipWithSum}
import chapter3.Tree.{depthWithFold, maxWithFold, size, sizeWithFold}
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

  "exercise 3.19" should "implement filter" in {
    assert(List.filter(List(1, 2, 4, 6, 7))(_ % 2 == 0) == List(2, 4, 6))
  }

  "append function" should "append a list at the end of other list" in {
    assert(append(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
  }

  "exercise 3.20" should "implement flatMap" in {
    assert(List.flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }

  "exercise 3.21" should "implement filter using flatmap" in {
    assert(filterWithFlatMap(List(1, 2, 4, 5, 6, 7))(_ % 2 == 0) == List(2, 4, 6))
  }

  "exercise 3.22" should "zipWithSum" in {
    assert(zipWithSum(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
  }

  "exercise 3.23" should "zipWith" in {
    assert(zipWith(List(1, 2, 3), List(2, 3, 4))(_ + _) == List(3, 5, 7))
  }

  protected val tree: Branch[Int] = Branch(
    Branch(
      Leaf(2),
      Leaf(13)
    ),
    Branch(
      Leaf(10),
      Branch(
        Leaf(3),
        Leaf(2)
      )
    )
  )

  protected val tree2 = Branch(
    Branch(
      Leaf(3),
      Leaf(14)
    ),
    Branch(
      Leaf(11),
      Branch(
        Leaf(4),
        Leaf(3)
      )
    )
  )

  "exercise 3.25" should "count the number of nodes in a tree" in {
    assert(size(tree) == 9)
  }

  "exercise 3.26" should "get the maximum element in a Tree[Int]" in {
    assert(Tree.max(tree) == 13)
  }

  "exercise 3.27" should "calculate maximum depth of a tree" in {
    assert(Tree.depth(tree) == 4)
  }

  "exercise 3.28" should "implement map for trees" in {
    assert(Tree.map(tree)(_ + 1) == tree2)
  }

  "exercise 3.29" should "implement fold for tree" in {
    assert(sizeWithFold(tree) == 9)
    assert(maxWithFold(tree) == 13 )
    assert(depthWithFold(tree) == 3)
//    assert(mapWithFold(tree) == tree2)
  }

}
