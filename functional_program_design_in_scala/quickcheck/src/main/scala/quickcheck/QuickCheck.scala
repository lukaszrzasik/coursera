package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val integers = arbitrary[Int]

  lazy val booleans = arbitrary[Boolean]

  lazy val genNonEmptyHeap: Gen[H] = for {
    element <- integers
    heap <- genHeap
  } yield insert(element, heap)

  lazy val genEmptyHeap: Gen[H] = const(empty)

  lazy val genHeap: Gen[H] = for {
    isEmpty <- booleans // Gen.frequency((9, false), (1, true))
    heap <- if (isEmpty) genEmptyHeap else genNonEmptyHeap
  } yield heap

  lazy val genNonEmptyList: Gen[List[Int]] = for {
    element <- integers
    list <- genList
  } yield element :: list

  lazy val genList: Gen[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) const(List()) else genNonEmptyList
  } yield list

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  implicit lazy val arbList: Arbitrary[List[Int]] = Arbitrary(genList)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (a min b)
  }

  property("delete1") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("ordered1") = forAll { heap: H =>
    def orderedList(heap: H, acc: List[A]): List[A] = {
      if (isEmpty(heap)) acc
      else {
        val min = findMin(heap)
        val newHeap = deleteMin(heap)
        orderedList(newHeap, min :: acc)
      }
    }
    val ordList = orderedList(heap, List())
    ordList == ordList.sorted.reverse
  }

  property("meld1") = forAll { (heap1: H, heap2: H) =>
    val melded = meld(heap1, heap2)
    if (isEmpty(melded)) isEmpty(heap1) && isEmpty(heap2)
    else {
      val min = findMin(melded)
      if (isEmpty(heap1)) min == findMin(heap2)
      else if (isEmpty(heap2)) min == findMin(heap1)
      else min == findMin(heap1) || min == findMin(heap2)
    }
  }

  property("delete2") = forAll { heap: H =>
    if (isEmpty(heap)) true
    else {
      val min = findMin(heap)
      findMin(insert(min, deleteMin(heap))) == min
    }
  }

  property("delete3") = forAll { list: List[Int] =>
    if (list.isEmpty) true
    else {
      val heap = list.foldLeft(empty)((h: H, a: Int) => insert(a, h))
      def orderedList(heap: H, acc: List[A]): List[A] = {
        if (isEmpty(heap)) acc
        else {
          val min = findMin(heap)
          val newHeap = deleteMin(heap)
          orderedList(newHeap, min :: acc)
        }
      }
      val ordList = orderedList(heap, List())
      ordList == list.sorted.reverse
    }
  }
}
