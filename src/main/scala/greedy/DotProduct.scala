package greedy

import java.util.Scanner
import scala.annotation.tailrec

object DotProduct {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val a = new Array[Long](n)
    val b = new Array[Long](n)

    for (i <- 0 until n) a(i) = s.nextLong()
    for (i <- 0 until n) b(i) = s.nextLong()

    val aSorted = a.toList.sorted.reverse
    val bSorted = b.toList.sorted.reverse

    // println(aSorted); println(bSorted)
    println(maxDotProduct(aSorted, bSorted, 0))
  }

  @tailrec
  def maxDotProduct(a: List[Long], b: List[Long], transport: Long): Long = {

    (a, b) match {
      case (Nil, _) | (_, Nil) => transport
      case (ha :: ta, hb :: tb) => maxDotProduct(ta, tb, transport + ha * hb)
    }
  }

}
