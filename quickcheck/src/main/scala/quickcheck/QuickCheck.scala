package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(a, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // If you insert any two elements into an empty heap, finding the minimum
  // of the resulting heap should get the smallest of the two elements back.
  property("minOfTwo") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val min = Math.min(a, b)
    findMin(h) == min
  }

  // If you insert an element into an empty heap, then delete the minimum,
  // the resulting heap should be empty.
  property("insDel") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  // Given any heap, you should get a sorted sequence of elements when
  // continually finding and deleting minima.
  property("sortedDels") = forAll { h: H =>
    def recDelete(heap: H, acc: List[Int]): List[Int] = {
      if (isEmpty(heap)) acc
      else recDelete(deleteMin(heap), acc :+ findMin(heap))
    }
    val mins = recDelete(h, List())
    mins == mins.sorted
  }

  // Finding a minimum of the melding of any two heaps should return a minimum
  // of one or the other.
  property("minMeld") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) true
    else if (isEmpty(h1)) findMin(meld(h1, h2)) == findMin(h2)
    else if (isEmpty(h2)) findMin(meld(h1, h2)) == findMin(h1)
    else {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      findMin(meld(h1, h2)) == m1 || findMin(meld(h1, h2)) == m2
    }
  }

  // The minimum of a three-element heap should be smaller than the minimum of
  // the two-element heap resulting from deleting the minimum of the three-element heap.
  property("insThree") = forAll { (a: Int) =>
    val h = insert(a+2, insert(a+1, insert(a, empty)))
    findMin(h) < findMin(deleteMin(h))
  }
}
