object lists {

  def last[A](la: List[A]): Option[A] =
    if(la.length > 0) Some(la(la.length - 1)) else None

  def penultimate[A](la: List[A]): Option[A] = la.length match {
    case 0 => None
    case 1 => Some(la(0))
    case _ => Some(la(la.length - 2))
  }

  def nth[A](n: Int, la: List[A]): Option[A] =
    if(la.length >= n) Some(la(n)) else None

  def length[A](la: List[A]): Int = {
    @annotation.tailrec
    def loop(cnt: Int, l: List[A]): Int = l match {
      case h :: Nil => cnt + 1
      case h :: t => loop(cnt + 1, t)
      case _ => cnt
    }

    loop(0, la)
  }

  def reverse[A](la: List[A]): List[A] =
    la.foldLeft(List[A]())((z, a) => a :: z)

  def isPalindrome[A](la: List[A]): Boolean = la == reverse(la)

  def flatten[A](lla: List[List[A]]): List[A] =
    lla.foldRight(List[A]())((la, z) => la ::: z)

  def compress[A](la: List[A]): List[A] =
    la.foldRight(List[A]())((a, z) => z match {
      case h :: t => if(a == h) z else a :: z
      case _ => a :: z
    })

  def pack[A](la: List[A]): List[List[A]] =
    la.foldRight(List[List[A]]())((a, z) => z match {
      case h :: t if a == h.head => a :: h; z // <-- I hate trailing z
      case _ => List(a) :: z
    })

  def encode[A](la: List[A]): List[(Int, A)] =
    pack(la).map(l => (l.length, l.head)) // inefficient

  def encodeModified[A](la: List[A]): List[Any] =
    encode(la).map(t => if(t._1 == 1) t._2 else t)

  def decode[A](l: List[(Int, A)]): List[A] =
    l.flatMap(t => List.fill(t._1)(t._2))

  def encodeDirect[A](la: List[A]): List[(Int, A)] =
    la.foldRight(List[(Int, A)]())((a, z) => z match {
      case h :: t if a == h._2 => (h._1 + 1, a) :: t
      case _ => (1, a) :: z
    })

  def duplicate[A](n: Int, la: List[A]): List[A] =
    la.flatMap(a => List.fill(n)(a))

  def drop[A](n: Int, la: List[A]): List[A] = {
      @annotation.tailrec
      def loop(i: Int, res: List[A]): List[A] = i % n match {
        case 0 => res
        case _ => la(i) :: res
      }

      if(la.length <= 1) la else loop(1, List(l.head))
  }

  def split[A](n: Int, la: List[A]): (List[A], List[A]) = {
    val len = la.length - n

    la.foldRight((List[A](),List[A]()))((a,z) => z._2.length match {
      case n if n <= len => a :: z._2
      case _ => a :: z._1
    })
  }

  def slice[A](i: Int, k: Int, la: List[A]): List[A] =
    split(k - i - 1, split(i - 1, la)._2)._1  // check index

  def rotate[A](n: Int, la: List[A]): List[A] =
    split(n, la).zipped.flatMap[A, List[A]]((e1, e2) => e2 ::: e1)

  def removeAt[A](i: Int, la: List[A]): (List[A], A) =
    (split(i - 1, la).zipped.flatMap[A, List[A]]((e1, e2) => e1 ::: e2.tail), la(i))


}