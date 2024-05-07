package chapter4

import org.scalatest.flatspec.AnyFlatSpec

class Chapter4Tests extends AnyFlatSpec{

  "exercise 4.1" should "implement Option::map" in {
    assert(Some(10).map((x: Int) => 2*x) == Some(20))
    assert(None.map((x: Int) => 2*x) == None)
  }

  "exercise 4.1" should "implement Option::flatMap" in {
    assert(Some(10).flatMap((x: Int) => Some(x)) == Some(10))
  }

  "exercise 4.1" should "implement Option::getOrElse" in {
    assert(Some(10).getOrElse(42) == 10)
    assert(None.getOrElse(42) == 42)
  }

  "exercise 4.1" should "implement orElse" in {
    assert(Some(10).orElse(Some(42)) == Some(10))
    assert(None.orElse(Some(42)) == Some(42))
  }

  "exercise 4.1" should "implement filter" in {
    assert(Some(10).filter(_ % 2 == 0) == Some(10))
    assert(Some(10).filter(_ % 2 == 1) == None)
  }

}
