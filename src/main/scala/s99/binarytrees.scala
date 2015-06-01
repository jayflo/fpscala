package binarytree {

  sealed abstract class Tree[+T]

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString =
      "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree {
    def cbalanced[T](n: Int, v: T): Tree[T] =
      Node(v, Tree.cbalanced(n / 2, v), Tree.cbalanced(n / 2 + 1, v))

    def isMirrorOf[_](t1: Tree[_], t2: Tree[_]): Boolean = (t1, t2) match {
      case (End, End) => true
      case ((Node(_,_,_), End) | (End, Node(_,_,_))) => false
      case t @ (Node(_, ll, lr), Node(_, rl, rr)) => (ll, lr, rl, rr) match {
        case (End, End, End, End) => true
        case (Node(_,_,_), End, End, Node(_,_,_)) => isMirrorOf(ll, rr)
        case (End, Node(_,_,_), Node(_,_,_), End) => isMirrorOf(lr, rl)
        case (Node(_,_,_), Node(_,_,_), Node(_,_,_), Node(_,_,_))
              => isMirrorOf(ll, rr) && isMirrorOf(lr, rl)
        case _ => false
      }
    }

    /*
    def isSymmetric: Boolean = this match {
      case n: Node[_] => isMirrorOf(n.left, n.right)
      case _ => true
    }
    */

    /*
    def addValue[T, U >: T <% Ordered[U]](x: U): Tree[U] = this match {
      case _: End => Node(x)
    }
    */
  }
}