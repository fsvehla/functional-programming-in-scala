package fpinscala.chapter3

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FreeSpec, ParallelTestExecution}

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

  "3.14 append as in foldLeft" in {
    val a = List(1, 2, 3)
    val b = List(4, 5, 6, 7)

    assert(List.appendWithFoldLeft(a, b) == List(1, 2, 3, 4, 5, 6, 7))
  }

  /* 3.15
   * Hard: Write a function that concatenates a list of lists into a single list.
   * Its runtime should be linear in the total length of all lists.
   * Try to use functions we have already defined.
   */

  "3.16 transformByAdding1" - {
    "adds 1 to each element" in {
      val intList = List(1, 2, 3, 4)
      assert(List.transformByAdding1(intList) === List(2, 3, 4, 5))
    }

    "adds 1 to each element when implemented with foldRight" in {
      val intList = List(1, 2, 3, 4)
      assert(List.transformByAdding1WithTailRec(intList) === List(2, 3, 4, 5))
    }
  }

  "3.17 transformDoubleToString" in {
    assert(
      List.transformDoubleToString(List(2.0, 5.5, 4.3)) ===
      List("2.0", "5.5", "4.3")
    )
  }

  "3.18 map" - {
    "maps over" in {
      assert(
        List.map(List(2, 4, 6)) { _ * 2 } ===
        List(4, 8, 12))
    }
  }

  "3.19 filter" in {
    assert(
      List.filter(List(1, 2, 3, 4, 5, 6)) { _ % 2 == 0 } ===
      List(2, 4, 6)
    )
  }

  "dafaq" in {
    assert(
      List.append(List(1, 2, 3), List(4, 5)) ===
      List(1, 2, 3, 4, 5)
    )
  }

  /**
   * “Write a function flatMap that works like map except that the function given will
   * return a list instead of a single result, and that list should be inserted into the
   * final resulting list
   */
  "3.20 flatMap" in {
    assert(
      List.flatMap(List(1, 2, 3)) { e ⇒ List(e, e) } ===
      List(1, 1, 2, 2, 3, 3)
    )
  }

  "3.21 filterWithFlatMap" in {
    assert(
      List.filterWithFlatMap(List(1, 2, 3, 4, 5, 6)) { _ % 2 == 0 } ===
        List(2, 4, 6)
    )
  }

  "3.22 intZip" in {
    assert(List.intZipAdd(Nil, Nil) === Nil)
    assert(List.intZipAdd(List(1, 2, 3), List(4, 5, 6))    === List(5, 7, 9))
    assert(List.intZipAdd(List(1, 2, 3), List(4, 5, 6, 7)) === List(5, 7, 9))
    assert(List.intZipAdd(List(1, 2, 3), List(4, 5))       === List(5, 7))
  }

  "3.23 zipWith" in {
    assert(List.zipWith(Nil, Nil) { (a, b) ⇒ a } === Nil)
    assert(List.zipWith(List(1.0, 1.2, 2.0), List(1, 2, 4)) { (l, r) ⇒ l * r } === List(1.0, 2.4, 8.0))
    assert(List.zipWith(List(1.0, 1.2), List(1, 2, 4)) { (l, r) ⇒ l * r } === List(1.0, 2.4))
    assert(List.zipWith(List(1.0, 1.2, 2.0), List(1, 2)) { (l, r) ⇒ l * r } === List(1.0, 2.4))
  }
}
