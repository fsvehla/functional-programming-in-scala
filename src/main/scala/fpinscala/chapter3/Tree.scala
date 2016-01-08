package fpinscala.chapter3

sealed trait Tree[+A]
final case class Leaf[A](value: A) extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[_](t: Tree[_]): Int =
    Tree.fold(t)(_ ⇒ 1)(1 + _ + _)

  def maximum[A](t: Tree[A])
    (implicit ordering: Ordering[A]): A =
    Tree.fold(t)(identity)(ordering.max(_, _))

  def depth[A](t: Tree[A]): Int =
    Tree.fold(t) { case leaf ⇒
      0
    } { case (x, y) ⇒
      1 + math.max(x, y)
    }

  def map[A](tree: Tree[A])(f: A ⇒ A): Tree[A] =
    Tree.fold(tree)(l ⇒ Leaf(f(l)).asInstanceOf[Tree[A]]) { case (x, y) ⇒
      // Why do we have not to recurse here? - because fold does it
      Branch(x, y)
    }

  def fold[A, B](tree: Tree[A])(leafFn: A => B)(branchFn: (B, B) ⇒ B): B = tree match {
    case Leaf(a)      ⇒ leafFn(a)
    case Branch(l, r) ⇒
      branchFn(
        fold(l)(leafFn)(branchFn),
        fold(r)(leafFn)(branchFn))
  }
}
