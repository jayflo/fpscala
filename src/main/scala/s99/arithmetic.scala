package arithmetic {
  import scala.language.implicitConversions

  class S99Int(val start: Int) {
    import S99Int._

    def isPrime: Boolean = {
      @annotation.tailrec
      def loop(i: Int): Boolean = start % i match {
        case 0 => false
        case _ => if(i * i > start) true else loop(i + 1)
      }

      loop(2)
    }

    def gcd(n: Int): Int = {
      @annotation.tailrec
      def loop(h: Int, l: Int, m: Int): Int = h - l match {
        case 0 => l
        case x if x > 0 => loop(x, l, m + 1)
        case _ => loop(l, m, 0)
      }

      loop(math.max(start, n), math.min(start, n), 0)
    }

    def isCoprime(n: Int): Boolean = gcd(n) == 1

    def totient: Int =
      (2 to start).foldLeft(1)((z,i) => if(start.isCoprime(i)) z + 1 else z)

    @annotation.tailrec
    private def multiplicity(p: Int, m:Int, n: Int): (Int, Int) = n % p match {
      case 0 => multiplicity(p, m + 1, n/p)
      case _ => (m, n)
    }

    def primeFactors: List[Int] = {
      @annotation.tailrec
      def loop(i: Int, n: Int, l: List[Int]): List[Int] = n match {
        case 1 => l
        case _ => {
          if(i.isPrime)
            multiplicity(i, 0, n) match {
              case (0, _) => loop(i + 1, n, l)
              case (im: Int, nn: Int) => loop(i + 1, nn, List.fill(im)(i) ::: l)
            }
          else loop(i + 1, n, l)
        }
      }

      loop(2, start, List(1)).reverse
    }

    def primeFactorMultiplicity: List[(Int,Int)] =
      (2 to start).foldLeft(List((1,1)))((z, i) =>
        multiplicity(i, 0, start) match {
          case (0,_) => z
          case (im: Int, _) => (i, im) :: z
        }).reverse

    def totientImp: Int =
      (2 to start).view
        .filter(i => i.isPrime)
        .foldLeft(1)((z,i) => multiplicity(i, 0, start) match {
          case (0,_) => z
          case (im: Int, _) => z * (i - 1) * math.pow(i, im - 1).toInt
        })

    def listPrimesInRange(l: List[Int]): List[Int] =
      l.filter(i => i.isPrime)

    def goldbach: (Int,Int) = {
      val half = start / 2;

      @annotation.tailrec
      def loop(i: Int): (Int,Int) =
        if(i.isPrime && (start - i).isPrime) (i, start - i)
        else if (i > half) (-1, -1)
        else loop(i + 1)

      loop(2)
    }

    def printGoldbachList(l: List[Int]): Unit = {
      def gbprint(n: Int, t: (Int,Int)): Unit =
        println(s"${n} = ${t._1} + ${t._2}")

      l.filter(i => i % 2 == 0).foreach(i => gbprint(i, i.goldbach))
    }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  }
}