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
    la.foldRight(List[List[A]]())((a, z) => z.headOption match {
      case Some(l) => { if(a == l.head) a :: l else List(a) :: z; z} // gah
      case None => List(a) :: z
    })

  def encode[A](la: List[A]): List[(Int, A)] =
    pack(la).map(l => (l.length, l.head)) // inefficient

  def decode[A](l: List[(Int, A)]): List[A] =
    l.flatMap(t => List.fill(t._1)(t._2))

  //def encodeDirect[A](la: List[A]): List[(Int, A)] =*/

  def duplicate[A](la: List[A]): List[A] =
    la.flatMap(a => List(a,a))

  // drops 0
  def drop[A](n: Int, la: List[A]): List[A] =
    la.view.zipWithIndex.filter(t => t._2 % n != 0).map(t => t._1).toList

  
}