package divideandconquer

import java.util.Scanner
import scala.annotation.tailrec

// Maybe implement a monad here

object Inversions {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val seq = new Array[Long](n)
    for (i <- 0 until n) seq(i) = s.nextLong()
    val (sorted, inversions) = mergeSort(seq.toList, 0)
    // println( sorted )
    println( inversions )

    /*
    12
    1 10 2 4 14 17 3 2 8 14 5 4

    1 10 2 4 14 17 3 2 8 14 5 4
      |  . .       . . .    . .
           |       . .
             |     . . .    . .
                |  . . . .  . .
                   | .
                       |    . .
                          | . .
                            | .

     . = inversion, so 26 inversions

    */
  }

  def mergeSort(seq: List[Long], inv: Int): (List[Long], Long) = {

    if (seq.length <= 1) (seq, inv)
    else {
      val mid = seq.length / 2
      val (left, right) = seq.splitAt(mid)
      val (sortedLeft, inv1) = mergeSort(left, inv)
      val (sortedRight, inv2) = mergeSort(right, inv)
      merge( sortedLeft, sortedRight, inv1 + inv2, (Nil,0) )
    }
  }

  @tailrec
  def merge(left: List[Long], right: List[Long], inv: Long, acc: (List[Long], Long)): (List[Long], Long) = {

    (left, right) match {
      case (seq, Nil) => (acc._1.reverse ::: seq, inv)
      case (Nil, seq) => (acc._1.reverse ::: seq, inv)
      case (hl::tl, hr::tr) =>
        if (hl <= hr) merge(tl, right, inv, (hl::acc._1, acc._2))
        else merge(left, tr, inv + left.length, (hr::acc._1, acc._2))
    }
  }

}
