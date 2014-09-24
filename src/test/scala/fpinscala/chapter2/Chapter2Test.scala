package fpinscala.chapter2

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FreeSpec
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration.{DurationDouble, DurationInt}

class Chapter2Test extends FreeSpec
  with TypeCheckedTripleEquals
  with TableDrivenPropertyChecks {

  "2.1 - Fibonacci" - {
    "Creates the correct numbers" in {
      val table = Table(
        ("nth", "expected"),
        (0, 0),
        (1, 1),
        (2, 1),
        (3, 2),
        (4, 3),
        (5, 5),
        (6, 8),
        (7, 13),
        (8, 21),
        (9, 34))

      forAll(table) { (nth, expected) =>
        assert(fibTailRec(nth) === expected)
      }
    }
  }

  "findFirst" - {
    "Returns the first element" in {
      assert(findFirst(Array("eins", "zwei", "drei"), "zwei") === 1)
      assert(findFirst(Array("eins", "zwei", "drei"), "nope") === -1)
    }
  }

  "findFirstP" - {
    "Returns the first element" in {
      assert(findFirstWithPredicate(Array("eins", "zwei", "drei")) { n ⇒  n(0) == 'z'} === 1)
      assert(findFirstWithPredicate(Array("eins", "zwei", "drei")) { n ⇒  n(0) == 'x'} === -1)
    }
  }

  "2.2 - Is sorted" - {
    "when passing a function" in {
      assert(isSorted(Array(1, 2, 3, 4)) { (a, b) ⇒ a <= b } === true)
      assert(isSorted(Array(1, 2, 2, 2)) { (a, b) ⇒ a <= b } === true)
      assert(isSorted(Array(1, 2, 2, 3)) { (a, b) ⇒ a <= b } === true)
      assert(isSorted(Array(1, 4, 3, 4)) { (a, b) ⇒ a <= b } === false)
    }

    "when using Ordering" in {
      assert(isSortedWithOrd(Array(1, 2, 3, 4)) === true)
      assert(isSortedWithOrd(Array(1, 2, 2, 2)) === true)
      assert(isSortedWithOrd(Array(1, 2, 2, 3)) === true)
      assert(isSortedWithOrd(Array(1, 4, 3, 4)) === false)

      assert(isSortedWithOrd(Array(1.milli, 1.5.millis, 2.millis, 3.millis)) === true)
    }
  }

  "2.3 - curry" - {
    "convert fn(1, 2) into fn(1)(2)" in {
      val curriedFn: (Int) ⇒ (Int) ⇒ Int = curry { (a: Int, b: Int) ⇒ a * 2 * b }
      assert(curriedFn(3)(5) === 30)
    }
  }

  "2.4 - uncurry" in {
    // A => (B => C) → A => B => C
    val uncurriedFn: (Int, Int) ⇒ Int =
      uncurry[Int, Int, Int]({ (a: Int) ⇒  { (b: Int) ⇒ (a * 2) * b }})

    assert(uncurriedFn(3, 5) === 30)
  }

  "2.5 - compose" - {
    val timesTwo:  (Int) ⇒ Int = _ * 2
    val timesFive: (Int) ⇒ Int = _ * 5

    "Works by hand" in {
      assert(compose(timesTwo)(timesFive)(2) === 20)
    }

    "Works when cheating" in {
      assert(composeCheat(timesTwo)(timesFive)(2) === 20)
    }
  }
}
