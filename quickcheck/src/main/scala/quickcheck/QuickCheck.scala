package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  /* Example 1 */
  lazy val genEmptyHeap: Gen[H] = const[H](empty)

  /* Example 2 */
  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    m <- oneOf[H](empty,genHeap)
  } yield insert(k, m)

  /* Example 3 */
  lazy val genMixHeap: Gen[H] = for {
    h1 <- genHeap
    h2 <- genHeap
  } yield meld(h1, h2)

  /* We use example 2 */
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should
    * get the smallest of the two elements back.
    */
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == math.min(a, b)
  }

  /**
    * If you insert an element into an empty heap, then
    * delete the minimum, the resulting heap should be empty.
    */
  property("empty") = forAll { a: Int =>
    deleteMin(insert(a, empty)) == empty
  }

  /**
    * Given any heap, you should get a sorted sequence
    * of elements when continually finding and deleting
    * minima. (Hint: recursion and helper functions are
    * your friends.)
    */
  property("sorted sequence") = forAll { h: H =>
    val seq: Seq[Int] = getSequence(h)
    // Compare each element to its following element
    (seq, seq.tail).zipped.forall(_ <= _)
  }

  /**
    * Finding a minimum of the melding of any two heaps
    * should return a minimum of one or the other.
    */
  property("min.meld") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == math.min(findMin(h1),findMin(h2))
  }

  property("associative meld") = forAll { (h: H, i: H, j: H) =>
    val a = meld(meld(h, i), j)
    val b = meld(h, meld(i, j))
    getSequence(a) == getSequence(b)
  }

  property("order of mins") = forAll { h: H =>
    getSequence(h).zip(getSequence(h).drop(1)).forall {
      case (x, y) => x <= y
    }
  }

  def getSequence(h: H): List[Int] = h match {
    case h if h == empty => Nil
    case h => findMin(h) :: getSequence(deleteMin(h))
  }

}
