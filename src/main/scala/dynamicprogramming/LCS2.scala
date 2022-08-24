package dynamicprogramming

import java.util.Scanner
import scala.annotation.tailrec

object LCS2 {

  def main(args: Array[String]): Unit = {

    val scan = new Scanner(System.in)

    val n = scan.nextInt()
    val s = new Array[Long](n)
    for (i <- 0 until n) s(i) = scan.nextLong()
    val k = scan.nextInt()
    val t = new Array[Long](k)
    for (i <- 0 until k) t(i) = scan.nextLong()

    /*
          5 2 8 7
          -------
        0 0 0 0 0
    2 | 0 0 1 1 1
    7 | 0 0 1 1 2
    8 | 0 0 1 2 2
    3 | 0 0 1 2 2

    */

    println(lcs2(s, t))
  }

  // recursive, using 2 rows (prevRow, curRow)
  def lcs2(s: Array[Long], t: Array[Long]): Int = {

    @tailrec
    def inner(s: Array[Long], t: Array[Long], prevRow: Array[Int]): Int = {

      val sChar = s.head
      val curRow = Array.fill(t.length+1)(0)

      // loop over all of t
      // pick corresponding value from prevRow and curRow
      // set curRow values
      for (i <- 1 to t.length) {
        val del = prevRow(i)
        val ins = curRow(i-1)
        val subs = if (sChar == t(i-1)) prevRow(i-1) + 1 else 0
        // pick maximum value
        curRow(i) = Set(del, ins, subs).max
      }

      /*
      println
      print(sChar)
      curRow.foreach(x => print(s" $x"))
      */

      if ( s.tail.isEmpty ) curRow(t.length) // we are done, return the last (lower right) element
      else inner(s.tail, t, curRow) // move down a row
    }

    // first call: prepare first row ( 0 0 0 0 0 etc. )
    val prevRow = Array.fill(t.length+1)(0)
    inner(s, t, prevRow)
  }

}
