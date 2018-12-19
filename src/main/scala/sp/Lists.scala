package sp

object Lists extends App{

//  def last(l: List[Int]): Int = { //find last item of list
//    if (l.tail.nonEmpty)
//      last(l.tail)
//    else
//      l.head
//  }

  def last(l: List[Any]): Any = l match {
    case x :: Nil => x
    case x :: xs  => last(xs)
    case _        => throw new NoSuchElementException
  }

//  def penultimate(l: List[Int]): Int = { //find second to last item of list
//    if (l.tail.tail.nonEmpty)
//      penultimate(l.tail)
//    else
//      l.head
//  }

  def penultimate(l: List[Any]): Any = l match {
    case x :: _ :: Nil => x
    case _ :: xs       => penultimate(xs)
    case _             => throw new NoSuchElementException
  }

//  def nth(l: List[Int], n: Int): Int = { //find nth element of list
//    if (n < 1)
//      nth(l.tail, n - 1)
//    else
//      l.head
//  }

  def nth(l: List[Int], n: Int): Int = (l, n) match {
    case (x :: Nil, 0) => x
    case (x :: xs, 0)  => x
    case (x :: xs, n)  => nth(xs, n - 1)
    case n             => throw new NoSuchElementException
    //case x            => throw new NoSuchElementException
  }

//  def reverse(rl: List[Any], l: List[Any]): List[Any] = { //reverse a list
//
//    if (l.tail.nonEmpty)
//      reverse(l.head :: rl, l.tail)
//    else {
//      l.head :: rl
//    }
//  }

  def reverse[T](rl: List[T], l: List[T]): List[T] = l match {
    case x :: Nil => x :: rl
    case x :: xs  => reverse(x :: rl, xs)
    case Nil      => Nil
  }

  //  def altReverse(revList: List[Int], normalList: List[Int]): List[Int] = { //reverse a list using pattern matching
  //    normalList match {
  //      case head :: Nil => head :: revList
  //      case head :: tail => altReverse(head :: revList, tail)
  //    }
  //  }

  def isPalindrome[T](l: List[T], rl: List[T]): Boolean = { //check if a list is a Palindrome
    val rl: List[Any] = reverse(List(), l) //(if a list is equal to its reverse)
    l == rl
  }

//  def flatten(l: List[Any], fl: List[Any]): List[Any] = {
//    if (l.tail.nonEmpty) {
//      flatten(l.tail, List(l.head) ::: fl)
//    } else
//      l :: fl
//  }

  def flatten1[T](l: List[List[T]]): List[T] = l match {
    case Nil     => Nil
    case x :: xs => x ++ flatten1(xs)
  }

  def flatten(l: List[Any]): List[Any] = l match {
    case Nil             => Nil
    case (x :: xs) :: ys => flatten(x :: xs) ++ flatten(ys)
    case x :: xs         => x :: flatten(xs)
  }

//  def compress(l: List[Symbol], cl: List[Symbol]): List[Any] = {
//    if (cl.isEmpty)
//      compress(l.tail, List(l.head))
//    else if (l.tail.nonEmpty) {
//      if (l.head == cl.head)
//        compress(l.tail, cl)
//      else
//        compress(l.tail, l.head :: cl)
//    } else {
//      if (l.head == cl.head)
//        reverse(List(), cl)
//      else
//        reverse(List(), l.head :: cl)
//    }
//  }

  def compress(l: List[Symbol], cl: List[Symbol]): List[Symbol] =
    (l, cl) match {
      case (x :: xs, Nil)       => compress(xs, List(x))
      case (x :: Nil, y :: Nil) => if (x == y) List(x, y) else List(y)
      case (x :: xs, y :: Nil) =>
        if (x == y) compress(xs, List(y)) else compress(xs, List(x,y))
      case (x :: Nil, y :: ys) => if (x == y)  (y :: ys).reverse else (y::ys).reverse
      case (x :: xs, y :: ys) => if (x == y)  compress(xs, y :: ys) else compress(xs, x::y::ys)
    }

  println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),List()))
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
  //println(flatten(List(List(1, 1), 2, List(3, List(5, 8))), List()))
  //val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  //println(flatten(l, List()))

  //val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  //println(compress(l, List()))
}
