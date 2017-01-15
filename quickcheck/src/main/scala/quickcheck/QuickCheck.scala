package quickcheck

import common._
import org.scalacheck._
import Gen._
import Arbitrary._
import Prop._
import math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    toAdd <- arbitrary[A]
    curHeap <- oneOf[H](empty, genHeap)
  } yield insert(toAdd, curHeap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("join two heaps") = forAll { (h1: H, h2: H) =>
    val m = min(findMin(h1), findMin(h2))
    findMin(meld(h1, h2)) == m
  }

  property("new element to empty") = forAll { (a: Int) =>
    findMin(insert(a, empty)) == a
  }

  property("3 new elements to empty") = forAll { (a: Int, b: Int, c: Int) =>
    findMin(insert(c, insert(b, insert(a, empty)))) == (a min b min c)
  }

  property("2 new elements to empty") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == (a min b)
  }

  property("not empty") = forAll { (a: Int) =>
    !isEmpty(insert(a, empty))
  }

  property("new element to empty, delete one") = forAll { (a: Int, b: Int) =>
    findMin(deleteMin(insert(b, insert(a, empty)))) == max(a, b)
  }

  property("empty back") = forAll { (a: Int) =>
    deleteMin(insert(a, empty)) == empty
  }

  property("findMin, delete, reinsert") = forAll { (h: H) =>
    val m = findMin(h)
    findMin(insert(m, deleteMin(h))) == m
  }

  property("put a new min, remove it") = forAll { (h: H, a: Int) =>
    val m = findMin(h)
    a > m || findMin(deleteMin(insert(a, h))) == m
  }

  property("take all out") = forAll { (h: H, a: Int) =>
    def rec(h: H): List[A] = if (isEmpty(h)) Nil else findMin(h) :: rec(deleteMin(h))
    rec(h) == rec(h).sorted
  }

  property("transferring the minimum to another heap") = forAll { (h1: H, h2: H) =>
    def rec(h: H): List[A] = if (isEmpty(h)) Nil else findMin(h) :: rec(deleteMin(h))

    val melded = meld(h1, h2)
    val transferred = meld(deleteMin(h1), insert(findMin(h1), h2))
    rec(melded) == rec(transferred)
  }

}
