package fpinscala.chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  val Nil = fpinscala.chapter3.Nil

  def sum(ints: List[Int]): Int = ints match {
    case Nil ⇒ 0
    case Cons(x, xs) ⇒ x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil ⇒ 1.0
    case Cons(0.0, _) ⇒ 0.0
    case Cons(x, xs)  ⇒ x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // 3.2
  def tail[A](ls: List[A]): List[A] = ls match {
    case Cons(h, t) ⇒ t
    case Nil        ⇒ Nil
  }

  // 3.3
  def setHead[A](ls: List[A], newHead: A): List[A] = ls match {
    case Cons(h, t) ⇒ Cons(newHead, t)
    case Nil        ⇒ Nil
  }

  // 3.4
  // note that this function takes time proportional only to
  // the number of elements being dropped.
  @tailrec
  def drop[A](ls: List[A], elements: Int): List[A] = (elements, ls) match {
    case (0, l)          ⇒ l
    case (n, Cons(h, t)) ⇒ drop(t, n - 1)
    case (_, Nil)        ⇒ Nil
  }

  // 3.4
  @tailrec
  def dropWhile[A](l: List[A])(fn: A ⇒ Boolean): List[A] = l match {
    case Nil ⇒ Nil
    case Cons(h, t) if fn(h)   ⇒ dropWhile(t)(fn)
    case nonMatch @ Cons(_, _) ⇒ nonMatch
  }

  // Code Listing
  private[this] def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  // 3.6
  def init[A](l: List[A]): List[A] = {
    @tailrec
    def doRecurse(remaining: List[A], acc: List[A]): List[A] = remaining match {
      case Nil          ⇒ acc
      case Cons(h, Nil) ⇒ acc // Ha-ha!
      case Cons(h, t)   ⇒ doRecurse(t, append(acc, apply(h)))
    }

    doRecurse(l, Nil)
  }

  def foldRight[A, B](list: List[A], init: B)(fn: (A, B) ⇒ B): B = list match {
    case Nil         ⇒ init // init
    case Cons(x, xs) ⇒ fn(x, foldRight(xs, init)(fn))
  }

  // Stolen from https://github.com/cevaris/fp-in-scala/blob/master/ch3/FoldLeft.scala
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(foldLeft(t, z)(f), h)
  }

  def length[A](as: List[A]): Int = foldRight(as, 0) { (_acc, item) ⇒ item + 1 }

//  def sumAsFoldLeft[A](l: List[A]) = foldLeft(l, 0) { case (i, acc) ⇒ acc + i }
  def sumIntsAsFoldLeft(l: List[Int]) = foldLeft(l, 0) { case (i, acc) ⇒ acc + i }

  def sumSummable[A](l: List[A])(implicit ev: Summable[A]) =
    foldLeft(l, ev.zero)(ev.plus)

  def productAsFoldLeft(l: List[Int]) = foldLeft(l, 1)   { case (i, acc) ⇒ i * acc }

  def reverse[A](list: List[A]): List[A] =
    foldRight(list, List[A]()) { case (e, acc) ⇒
      append(acc, List(e))
    }
}

trait Summable[A] {
  val zero: A
  val plus: (A, A) ⇒ A
}

object Summables {
  implicit object IntSummable extends Summable[Int] {
    type A = Int

    override val zero: IntSummable.A = 0
    override val plus: (IntSummable.A, IntSummable.A) ⇒ IntSummable.A = { (a, b) ⇒ a + b}
  }

  implicit object DoubleSummable extends Summable[Double] {
    type A = Double

    override val zero: A = 0
    override val plus: (A, A) ⇒ A = { (a, b) ⇒ a + b}
  }
}
