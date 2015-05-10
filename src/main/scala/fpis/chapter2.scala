
object Chapter2 {

  /* 2.1 */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, count: Int): Int = 
      if(count == 0) a
      else if (count == 1) a + b
      else go(b, a + b, count - 1)

    go(0, 1, n)
  }

  /* 2.2 */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if(n >= as.length - 2) true
      else if(!ordered(as(n), as(n + 1))) false
      else loop(n + 1)

    loop(0)
  }

  /* 2.3 */
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a,b)

  /* 2.4 */
  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a: A, b: B) => f(a)(b)

  /* 2.5 */
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}