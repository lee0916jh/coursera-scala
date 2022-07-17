package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] =
    for {
      a <- arbitrary[Int]
      h <- oneOf(const(empty),genHeap)
    } yield insert(a,h)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a:Int, b:Int) =>
    val h = insert(a,empty)
    val h2 = insert(a,h)
    if (a < b)
      findMin(h2) == a
    else
      findMin(h2) == b
  }

  property("empty") = forAll { (a:Int) =>
    val h = insert(a, empty)
    val h1= deleteMin(h)
    isEmpty(h1)
  }

  property("meld") = forAll { (h1:H,h2:H)=>
    val melded = meld (h1,h2)
    val meldedMin = findMin(melded)
    meldedMin==findMin(h1) || meldedMin==findMin(h2)
  }

  property("isSorted") = forAll { (h:H)=>
    def isSorted(h:H):Boolean=
      if (isEmpty(h))
        true
      else
        val m = findMin(h)
        val h2 = deleteMin(h)
        if (isEmpty(h2))
          true
        else
          val m2 = findMin(h2)
          if (m2 > m)
            false
          else
            isSorted(deleteMin(h2))
    isSorted(h)
  }