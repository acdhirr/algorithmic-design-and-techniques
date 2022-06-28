package divideandconquer

import java.util.Scanner
import scala.annotation.tailrec

object BinarySearch {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val a = new Array[Long](n)
    for (i <- 0 until n) a(i) = s.nextLong()
    val k = s.nextInt()
    val b = new Array[Long](k)
    for (i <- 0 until k) b(i) = s.nextLong()

    val to = a.length
    b.foreach(q => print(s"${binarySearch(q, 0, to-1, a)} "))

  }

  @tailrec
  def binarySearch(q: Long, from: Int, to: Int, values: Array[Long]): Int = {

    if (from > to) -1 // exhausted
    else {
      val mid = (from+to)/2
      if (values(mid) == q) mid // found!
      else if (values(mid) > q)
        binarySearch(q, from, mid-1, values)
      else
        binarySearch(q, mid+1, to, values)
    }

  }

}
