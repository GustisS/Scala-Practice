package sp

object Lists extends App {
  def last(l: List[Int]): Int = { //find last item of list
    if (l.tail.nonEmpty)
      last(l.tail)
    else
      l.head
  }

  def penultimate(l: List[Int]): Int = { //find second to last item of list
    if (l.tail.tail.nonEmpty)
      penultimate(l.tail)
    else
      l.head
  }

  def nth(l: List[Int], n: Int): Int = { //find nth element of list
    if (n < 1)
      nth(l.tail, n - 1)
    else
      l.head
  }

  def reverse(rl: List[Any], l: List[Any]): List[Any] = { //reverse a list

    if (l.tail.nonEmpty) {
      reverse(l.head :: rl, l.tail)
    }
    else {
      l.head :: rl
    }
  }

  //  def altReverse(revList: List[Int], normalList: List[Int]): List[Int] = { //reverse a list using pattern matching
  //    normalList match {
  //      case head :: Nil => head :: revList
  //      case head :: tail => altReverse(head :: revList, tail)
  //    }
  //  }

  def isPalindrome(l: List[Any], rl: List[Any]): Boolean = { //check if a list is a Palindrome
    val rl: List[Any] = reverse(l, List()) //(if a list is equal to its reverse)
    l == rl
  }

  def flatten(l: List[Any], fl: List[Any]): List[Any] = {
    if (l.tail.nonEmpty) {
      flatten(l.tail, List(l.head) ::: fl)
    }
    else
      l :: fl //incomplete
  }


  def compress(l: List[Symbol], cl: List[Symbol]): List[Any] = {

    if (cl.isEmpty)
      compress(l.tail, List(l.head))
    else if (l.tail.nonEmpty) {
      if (l.head == cl.head)
        compress(l.tail, cl)
      else
        compress(l.tail, l.head :: cl)
    }
    else {
      if (l.head == cl.head)
        reverse(List(), cl)
      else
        reverse(List(), l.head :: cl)
    }
  }

  //def pack(l: List[List[Symbol]])

//
//  sealed trait List[T]
//  case class ::[T](head: T, tail : List[T]) extends List[T]
//  case object Nil extends List[Nothing]
//
//  def count(l : List[Int]): Int = l match {
//
//    case Nil => 0
//    case x :: xs => 1 + count(xs)
//    case x :: Nil => 1
//
//  }

  //def pack(l: List[Symbol]): List[Symbol] = {}
  println(flatten(List(List(1, 1), 2, List(3, List(5, 8))),List()))
  //val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  //println(flatten(l, List()))

  //val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  //println(compress(l, List()))
}
