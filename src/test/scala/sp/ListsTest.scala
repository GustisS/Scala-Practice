package sp

import org.scalatest.FunSuite
import sp.Lists._

class ListsTest extends FunSuite {

  val bl: List[Int] = List(-10, -2147483648, -1, 0, 1, 2147483647, 10) //boundary list
  val nl = List(List(1, 1), 2, List(3, List(5, 8))) //nested list
  val sl: List[Symbol] =
    List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  test("Last element of a list") {
    assert(last(List(3)) === 3)
    assert(last(List(2, 3)) === 3)
    assert(last(List(1, 2, 3)) === 3)
    intercept[NoSuchElementException] {
      last(List())
    }
    assert(last(bl) === 10)
    assert(last(sl) === 'e)
  }

  test("Penultimate element of a list") {
    intercept[NoSuchElementException] {
      penultimate(List(3))
    }
    assert(penultimate(List(2, 3)) === 2)
    assert(penultimate(List(1, 2, 3)) === 2)
    assert(penultimate(bl) === 2147483647)
    assert(penultimate(sl) === 'e)
  }

  test("Nth element of a list") {
    assert(nth(bl, 2) === -1)
    assert(nth(bl, 0) === -10)
    assert(nth(bl, 6) === 10)
    intercept[NoSuchElementException] {
      nth(bl, 7)
    }
  }

  test("Reverse a list") {
    assert(reverse(List(), List(1, 2)) === List(2, 1))
    assert(reverse(List(), List(1, 2)) === List(1, 2).reverse)
    assert(reverse(List(), reverse(List(), List(1, 2))) === List(1, 2))
  }

  test("Flatten a list") {
    assert(
      flatten(List(List(1, 1), 2, List(3, List(5, 8))))
        === List(1, 1, 2, 3, 5, 8))
  }

  test("Compress a list") {
    assert(
      compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),
               List()) === List('a, 'b, 'c, 'a, 'd, 'e))
  }

}
