package dynamicprogramming

import java.util.Scanner
import scala.annotation.tailrec

object EditDistance {

  // For checking outcomes use the online calculator at https://planetcalc.com/1721/

  def main(args: Array[String]): Unit = {

    val scan = new Scanner(System.in)
    val s = scan.next()
    val t = scan.next()

    /*
          d i s t a n c e  → t
          ---------------
        0 1 2 3 4 5 6 7 8
    e | 1 1 2 3 4 5 6 7 7
    d | 2 1 2 3 4 5 6 7 8
    i | 3 2 1 2 3 4 5 6 7
    t | 4 3 2 2 2 3 4 5 6
    i | 5 4 3 3 3 3 4 5 6
    n | 6 5 4 4 4 4 3 4 5
    g | 7 6 5 5 5 5 4 4 5

    ↓
    s

    */

    println( editDistance(s, t) )
  }

  // recursive, using 2 rows (prevRow, curRow)
  def editDistance(s: String, t: String): Int = {

    @tailrec
    def inner(s: String, t: String, prevRow: Array[Int], rowNr: Int): Int = {

      val sChar = s.head
      val curRow = rowNr +: new Array[Int](t.length)

      // loop over all of t
      // pick corresponding value from prevRow and curRow
      // set curRow values
      for (i <- 1 to t.length) {
        val del = prevRow(i) + 1
        val ins = curRow(i - 1) + 1
        val subs = if (sChar == t(i - 1)) prevRow(i - 1) else prevRow(i - 1) + 1
        // pick minimum value
        curRow(i) = Set(del, ins, subs).min
      }

      /*
      println
      print(sChar)
      curRow.foreach(x => print(s" $x"))
      */

      if (s.tail == "") curRow(t.length) // we are done, return the last (lower right) element
      else inner(s.tail, t, curRow, rowNr + 1) // move down a row
    }

    // first call: prepare first row ( 0 1 2 3 4 5 6 7 etc.)
    // and start at next row
    val prevRow = (0 to t.length).toArray
    val rowNr = 1

    inner(s, t, prevRow, rowNr)
  }

}
