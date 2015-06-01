package logic {
  object S99Logic {

    def and(a: Boolean, b: Boolean): Boolean = a && b
    def or(a: Boolean, b: Boolean): Boolean = a || b
    def nand(a: Boolean, b: Boolean): Boolean = !and(a, b)
    def nor(a: Boolean, b: Boolean): Boolean = !or(a, b)
    def xor(a: Boolean, b: Boolean): Boolean = or(and(a, !b), and(!a, b))
    def impl(a: Boolean, b: Boolean): Boolean = a // check
    def equ(a: Boolean, b: Boolean): Boolean = or(and(a, b), and(!a, !b)) //chk

    def table2(f: (Boolean, Boolean) => Boolean): Unit = {
      val tStr = "true  "
      val fStr = "false "

      def printResult(aStr: String, bStr: String, res: Boolean): Unit =
        println(s"${aStr} ${bStr} ${res}")

      println("A     B     result")
      printResult(tStr, tStr, f(true, true))
      printResult(tStr, fStr, f(true, false))
      printResult(fStr, tStr, f(false, true))
      printResult(fStr, fStr, f(false, false))
    }

    def gray(n: Int): List[String] = {
      @annotation.tailrec
      def loop(i: Int, ls: List[String]): List[String] = i match {
        case 1 => ls
        case _ => loop(
          i - 1, ls.map(s => "0" + s) ::: ls.reverse.map(s => "1" + s)
        )
      }

      loop(n, List("0", "1"))
    }

    //def huffman(lt: List[(String, Int)]): List[(String, String)] = {}

  }
}