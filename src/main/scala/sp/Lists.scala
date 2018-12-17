package sp

class Lists {
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
      nth(l.tail, n-1)
    else
      l.head
  }

  def reverse(revList: List[Int], normalList: List[Int]): List[Int] = { //reverse a list

    if (!normalList.tail.isEmpty) {
      reverse(normalList.head :: revList, normalList.tail)
    }
    else {
      normalList.head :: revList
    }
  }

  def altReverse(revList: List[Int], normalList: List[Int]): List[Int] = { //reverse a list using pattern matching
    normalList match {
      case head :: Nil => head :: revList
      case head :: tail => altReverse(head :: revList, tail)
    }
  }



}
