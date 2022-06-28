package divideandconquer

import java.util.Scanner

object MajorityElement {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val seq = new Array[Long](n)
    for (i <- 0 until n) seq(i) = s.nextLong()

    // 1: there is a majority element; -1: there is none
    println( if (majorityElement(seq, 0, seq.length-1 ) > -1) 1 else 0 )
  }

  def majorityElement(seq: Array[Long], from: Int, to: Int ): Long = {

    if (to == from) seq(to) // a single element is always a majority
    else {

      val mid = (from + to)/2 // --------------------------- divide
      val leftWinner = majorityElement(seq, from, mid)
      val rightWinner = majorityElement(seq, mid+1, to)
      val majority = (to+1-from)/2 +1 // half the segment size, plus 1

      if (countNumInSeq(leftWinner, seq, from, to) >= majority) leftWinner
      else if (countNumInSeq(rightWinner, seq, from, to) >= majority) rightWinner
      else -1 // it's a tie
    }
  }

  def countNumInSeq(num: Long, seq: Array[Long], from: Int, to: Int ): Int =
    seq.slice(from, to+1).toList.foldLeft(0)((a, b) => if (b==num) a+1 else a )

}
