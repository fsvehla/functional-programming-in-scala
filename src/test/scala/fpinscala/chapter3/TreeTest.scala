package fpinscala.chapter3

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FreeSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class TreeTest extends FreeSpec
  with TypeCheckedTripleEquals
  with TableDrivenPropertyChecks {

  import fpinscala.chapter3.Tree._

  "Tree" - {
    "size" in {
      assert(
        size(
          Branch(
            Branch(Leaf(1), Leaf(1)),
            Leaf(1))
        ) === 5)

      assert(size(Branch(Leaf(1), Leaf(1))) === 3)
    }

    "3.26 - maximum finds the maximum" in {
      val t =
        Branch(
          Branch(
            Leaf(15),
            Branch(Leaf(1), Leaf(9))),
          Branch(
            Branch(Leaf(20), Leaf(0)),
            Leaf(19)))

      assert(maximum(t) === 20)
    }

    "3.27 - depth finds the maximum path length from the root of a trea to any leaf" in {
      val t = Branch(
        Branch(
          Leaf(15),
          Branch(Leaf(1), Leaf(9))),
        Branch(
          Branch(Leaf(20), Leaf(0)),
          Leaf(19)))

      assert(depth(t) === 3)
    }

    "3.28 - Write a function `map`" in {
      val in = Branch(
        Branch(
          Leaf(15),
          Branch(Leaf(1), Leaf(9))),
        Branch(
          Branch(Leaf(20), Leaf(0)),
          Leaf(19)))

      val expected = Branch(
        Branch(
          Leaf(16),
          Branch(Leaf(2), Leaf(10))),
        Branch(
          Branch(Leaf(21), Leaf(1)),
          Leaf(20)))

      assert(Tree.map(in)(_ + 1) === expected)
      assert(Tree.map(Leaf(1))(_ * 8) === Leaf(8))
    }

    "3.29 - fold" in {
      /**
        * No hint that we need 2 functions - 1 for leaves,
        * and 1 for branches, but this makes sense in hindsight.
        */

      def sizeWithFold(tree: Tree[_]): Long = Tree.fold(tree)(_ ⇒ 1) {
        case (x, y) ⇒ 1 + x + y
      }

      assert(sizeWithFold(Leaf(1)) === 1L)

      val size5Tree = Branch(
          Branch(Leaf(1), Leaf(1)),
          Leaf(1))

      assert(size(size5Tree)         === 5)
      assert(sizeWithFold(size5Tree) === 5L)
    }
  }
}
