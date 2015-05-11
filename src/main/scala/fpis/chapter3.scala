
object Chapter3 {

  /* 3.1 */

  /* 3.2 */
  def tail[A](l: List[A]): List[A] =
    l match {
      case h :: t => t
      case _ => List()
    }

  /* 3.3 */
  def setHead[A](l: List[A], x: A): List[A] =
    l match {
      case h :: t => x +: t
      case _ => List(x)
    }

  /* 3.4 */
  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(as: List[A], i: Int): List[A] =
      (as, i) match {
        case (_, 0) => as
        case (h :: t, _) => loop(t, i - 1)
        case _ => List()
      }

    loop(l, n)
  }

  /* 3.5 */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def loop(as: List[A]): List[A] =
      as match {
        case h :: t => if(f(h)) loop(t) else as
        case _ => List()
      }

    loop(l)
  }

  /* 3.6 */
  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(as: List[A], tail: List[A]): List[A] =
      tail match {
        case h :: t => loop(as :+ h, t)
        case _ => as
      }

    loop(List(), l)
  }

  /* 3.7 */
  /* 3.8 */

  /* 3.9 */
  def length[A](as: List[A]): Int =
    as.foldRight(0)((_,i) => i + 1)

  /* 3.10 */
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    as match {
      case Nil => z
      case h :: t => foldLeft(t, f(z,h))(f)
    }

  /* 3.11 */
  def sum(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((z,_) => z + 1)

  /* 3.12 */
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((z,a) => a +: z)

  /* 3.13 */

  /* 3.14 */
  def append[A](as: List[A], al: List[A]): List[A] =
    foldLeft(al, as)((z,a) => z :+ a)

  def append2[A](as: List[A], al: List[A]): List[A] =
    as.foldRight(al)((a,z) => a +: z) // O(1)

  /* 3.15 */
  def concatenate[A](as: List[List[A]]): List[A] =
    as.foldRight(List[A]())((a,z) => append2(a, z))

  /* 3.16 */
  def translate(ns: List[Int], n: Int): List[Int] =
    foldLeft(ns, List[Int]())((z,a) => z :+ (a + n))

  /* 3.17 */
  def doubleToString(ns: List[Double]): List[String] =
    foldLeft(ns, List[String]())((z,a) => z :+ a.toString)

  /* 3.18 */
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldLeft(as, List[B]())((z,a) => z :+ f(a))

  /* 3.19 */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as.foldRight(List[A]())((a,z) => if(f(a)) a +: z else z)

  /* 3.20 */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    as.foldRight(List[B]())((a,z) => append2(f(a), z))

  /* 3.21 */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if(f(a)) List(a) else List())

  /* 3.22 */
  /* 3.23 */
  def zipWith[A](as: List[A], al: List[A])(f: (A,A) => A): List[A] = {
    @annotation.tailrec
    def loop(x: List[A], y: List[A], ret: List[A]): List[A] =
      (as, al) match {
        case (Nil, Nil) => ret
        case (h1 :: t1, h2 :: t2) => loop(t1, t2, ret :+ f(h1, h2))
        case _ => Nil
      }

    loop(as, al, List[A]())
  }

  /* 3.24 */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup.scanLeft(sup)((z,a) => tail(z))
      .filter(a => a == sub).length > 0

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  /* 3.25 */
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case b: Branch[_] => 1 + size(b.left) + size(b.right)
    }

  /* 3.26 */
  def maximum(t: Tree[Int]): Int =
    t match {
      case l: Leaf[Int] => l.value
      case b: Branch[Int] => maximum(b.left) max maximum(b.right)
    }

  /* 3.27 */
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case b: Branch[_] => 1 + (depth(b.left) max depth(b.right))
    }

  /* 3.28 */
  def mapT[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case l: Leaf[A] => Leaf(f(l.value))
      case b: Branch[A] => Branch(mapT(b.left)(f), mapT(b.right)(f))
    }

  /* 3.29 */
  def fold[A,B](t: Tree[A], z: B)(f: (B,Tree[A]) => B): B =
    t match {
      case l: Leaf[A] => f(z, l)
      case b: Branch[A] => f(fold(b.right, fold(b.left, z)(f))(f), b)
    }

  // def mapT2[A,B](t: Tree[A])(f: A => B): Tree[B] =
  //   fold(t, _)((_,a) => a match {
  //     case l: Leaf[_] => Leaf(f(l.value))
  //     case b: Branch[_] => Branch(mapT2(b.left)(f), mapT2(b.right)(f))
  //   })
}