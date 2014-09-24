package fpinscala

import scala.annotation.tailrec

package object chapter2 {
  // 2.1
  def fibTailRec(n: Int): Int = {
    @tailrec
    def doFibTailRec(n: Int, slidingAcc: Int, acc: Int): Int = n match {
      case 0 => slidingAcc
      case _ => doFibTailRec(n - 1, acc, slidingAcc + acc)
    }

    doFibTailRec(n, 0, 1)
  }

  def findFirst(ss: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) {
        -1
      } else if (ss(n) == key) {
        n
      } else {
        loop(n + 1)
      }

    loop(0)
  }

  def findFirstWithPredicate[A](as: Array[A])(p: A ⇒ Boolean) : Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) {
        -1
      } else if (p(as(n))) {
        n
      } else {
        loop(n + 1)
      }
    }

    loop(0)
  }

  // 2.2
  def isSorted[A](as: Array[A])(ordered: (A, A) ⇒ Boolean): Boolean = {
    @tailrec
    def walk(index: Int, last: A): Boolean = {
      if (index >= as.length) {
        true
      } else {
        if(ordered(last, as(index))) {
          walk(index +1, as(index))
        } else {
          false
        }
      }
    }

    walk(1, as(0))
  }

  // 2.2
  def isSortedWithOrd[A: Ordering](as: Array[A]): Boolean = {
    val ordered = implicitly[Ordering[A]]

    @tailrec
    def walk(index: Int, last: A): Boolean = {
      if(index >= as.length) {
        true
      } else {
        val current = as(index)

        if(ordered.lteq(last, current)) {
          walk(index + 1, current)
        } else {
          false
        }
      }
    }

    walk(1, as(0))
  }

  // 2.3
  def curry[A, B, C](fn: (A, B) ⇒ C): A ⇒ (B ⇒ C) =
    { (a: A) ⇒
      { (b: B) ⇒
        fn(a, b) } }

  // 2.4
  def uncurry[A, B, C](fn: A ⇒ (B ⇒ C)): (A, B) ⇒ C = {
    { (a: A, b: B) ⇒
      fn(a)(b) } }

  // 2.5
  def compose[A, B, C](f: B ⇒ C)(g: A ⇒ B): A ⇒ C = { (a:A) ⇒ f(g(a)) }
  def composeCheat[A, B, C](f: B ⇒ C)(g: A ⇒ B): A ⇒ C = f compose g
}
