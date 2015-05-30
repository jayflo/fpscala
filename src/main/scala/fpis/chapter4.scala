
object Chapter4 {

  /* 4.1 */
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] =
      this match {
        case None => None
        case s: Some[A] => Some(f(s.get))
      }

    def flatMap[B](f: A => Option[B]): Option[B] =
      this match {
        case None => None
        case s: Some[A] => f(s.get)
      }

    def getOrElse[B >: A](default: => B): B =
      this match {
        case None => default
        case s: Some[B] => s.get
      }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      this match {
        case None => ob
        case s: Some[B] => s
      }

    def filter(f: A => Boolean): Option[A] =
      this match {
        case None => this
        case s: Some[A] => if(f(s.get)) s else None
      }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  /* 4.2 */
  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  /* 4.3 */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a.flatMap(ag => b.map(bg => f(ag,bg)))

  /* 4.4 */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    Some(a.foldRight(List[A]())((ao, z) => ao match {
      case None => z
      case s: Some[A] => s.get +: z
    })).filter(al => al.length == a.length)

  /* 4.5 */
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    Some(a.foldRight(List[B]())((aa, z) => f(aa) match {
      case None => z
      case s: Some[B] => s.get +: z
      })).filter(al => al.length == a.length)

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(ao => ao)
    
  /* 4.6 */
  // sealed trait Either[+E,+A] {
  //   def map[B](f: A => B): Either[E, B] =
  //     this match {
  //       case r: Right[_] => Right(f(r.value))
  //       case x: Either[E,B] => x
  //     }

  //   def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE,B] =


  //   def orElse[EE >: E, B >: A](b: => Either[EE,B]): Either[EE,B]
  //   def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C]
  // }

  // case class Left[+E](value: E) extends Either[E, Nothing]
  // case class Right[+A](value: A) extends Either[Nothing, A]

}