package greedy

import java.util.Scanner
import scala.annotation.tailrec

object DifferentSummands {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val summands = optimalSummands(n,1, List[Int]()).reverse

    println(summands.length)
    summands.foreach(s => print(s"$s "))
  }

  @tailrec
  def optimalSummands(n: Int, summand: Int, summands: List[Int]): List[Int] = {

    if (summand >= n.toDouble/2) n :: summands // there are no more larger numbers left so append n as the last summand
    else optimalSummands(n-summand, summand+1, summand :: summands)
  }

}
