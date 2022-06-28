package divideandconquer

import java.util.Scanner
import scala.annotation.tailrec

object MergeSort {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val seq = new Array[Long](n)
    for (i <- 0 until n) seq(i) = s.nextLong()
    println( mergeSort(seq.toList) )

    /*
    12
    1 10 2 4 14 17 3 2 8 14 5 4
    */
  }

  def mergeSort(seq: List[Long]): List[Long] = {

    if (seq.length <= 1) seq
    else {
      val mid = seq.length / 2
      val (seq1, seq2) = seq.splitAt(mid)
      merge( mergeSort(seq1), mergeSort(seq2), Nil )
    }
  }

  @tailrec
  def merge(seq1: List[Long], seq2: List[Long], acc: List[Long]): List[Long] = {

    (seq1, seq2) match {
      case (s, Nil) => acc.reverse ::: s
      case (Nil, s) => acc.reverse ::: s
      case (h1::t1, h2::t2) =>
        if (h1 <= h2) merge(t1,seq2,h1::acc)
        else merge(seq1,t2,h2::acc)
    }
  }

}
