package fpinscala.chapter3

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{FreeSpec, FunSuite}
import org.scalatest.prop.TableDrivenPropertyChecks

class ListTest extends FreeSpec
  with TypeCheckedTripleEquals
  with TableDrivenPropertyChecks {

  import List._
  val Nil = fpinscala.chapter3.Nil

  "3.2 tail" - {
    "returns the tail of the list" in {
      assert(tail(List(1, 2, 3)) ===
        List(2, 3))

      assert(tail(List(1, 2)) ===
        List(2))

      assert(tail(List(1)) === Nil)
      assert(tail(Nil)     === Nil)
    }
  }

  "3.3 setHead" - {
    "replaces the head of a list" in {
      assert(setHead(List(1, 6, 9), 3) ===
        List(3, 6, 9))

      assert(setHead(List(), 3) === Nil)
    }
  }

  "3.4 drop" - {
    "drops the n elements of the list" in {
      val list = List(1, 2, 3, 4)
      assert(drop(list, 1)       === List(2, 3, 4))
      assert(drop(list, 3)       === List(4))
      assert(drop(list, 4)       === Nil)
      assert(drop(list, 5)       === Nil)
      assert(drop(List(1, 2), 1) === List(2))
      assert(drop(List(1), 1)    === Nil)
      assert(drop(Nil, 1)        === Nil)
    }
  }

  "3.5 dropWhile" in {
    val list = List(1, 2, 3, 4,  5)

    assert(dropWhile(list) { _ <= 3 } === List(4, 5))
    assert(dropWhile(list) { _ > 3 }  === List(1, 2, 3, 4, 5))
  }

  /**
   * Not everything works out so nicely. Implement a function, init, that returns a List consisting of all but the last element of a List. So, given List(1,2,3,4), init will return List(1,2,3). Why can’t this function be implemented in constant time like tail?
   */

  "3.6 init" in {
    val list = List(1, 2, 3, 4)
    assert(init(list) === List(1, 2, 3))
  }

  "3.9 length" in {
    val table = Table(
      ("List",  "Expected"),
      (Nil,     0),
      (List(1), 1),
      (List(1, 2), 2))
  }

  "3.11 sumFoldLeft" - {
    "returns the sum for ints" in {
      assert(List.sumIntsAsFoldLeft(List(1, 5, 3)) === 9)
    }

    "returns the sum for anything summable" in {
      import Summables._

      assert(List.sumSummable(List(1,   2,   3))   === 6)
      assert(List.sumSummable(List(1.0, 2.0, 3.0)) === 6.0)
    }
  }

  "3.11 productFoldLeft" - {
    "returns the product" in {
      assert(List.productAsFoldLeft(List(5, 2, 3)) === 30)
    }
  }

  "3.11 lengthFoldLeft" in {
    info("Already implemented this way")
  }

  "3.12 reverse" in {
    assert(List.reverse(List(1, 2, 3)) === List(3, 2, 1))
    assert(List.reverse(List(1, 2))    === List(2, 1))
    assert(List.reverse(List(1))       === List(1))
    assert(List.reverse(List())        === List())
  }
}
