package dynamicprogramming

import java.util.Scanner
import scala.annotation.tailrec

object PrimitiveCalculator {

  /* In the array at position i we store the number of operations to get
     at number i, along with number the last op was applied on (the operation
     itself (*3, *2 or -1) is not stored. */
  case class Operation(ops: Int, prev: Int) {
    override def toString: String = s"ops = $ops, prev = $prev" // for inspection/debugging
  }

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val seq = optimalSequence(n)
    val trace = retrace(seq, n, List())

    // number of operations needed
    println(seq(n).ops)
    // numbers involved
    trace.foreach(x => print(s"$x "))
  }

  // Get the list of participating numbers by following the prev field in each Operation
  def retrace(mem: Array[Operation], index: Int, acc: List[Int]): List[Int] =
    if (index == 0) acc
    else retrace(mem, mem(index).prev, index :: acc)

  // x*2 x*3 x+1
  def optimalSequence(m: Int): Array[Operation] = {

    val mem = new Array[Operation](m+1) // initialize with -1
    mem(0) = Operation(-1,0)
    mem(1) = Operation(0,0)

    @tailrec // just looping n to m
    def inner(n: Int): Array[Operation] = {

      if (n > m) mem // done
      else {

        val op3 = if (n % 3 == 0) mem(n/3).ops + 1 else Int.MaxValue
        val op2 = if (n % 2 == 0) mem(n/2).ops + 1 else Int.MaxValue
        val op1 = mem(n - 1).ops + 1

        // find minimum
        if (op1 <= op2 && op1 <= op3)
          mem(n) = Operation(op1, n-1) // at n store number of steps and previous value
        else if (op2 <= op1 && op2 <= op3)
          mem(n) = Operation(op2, n/2) // id.
        else
          mem(n) = Operation(op3, n/3) // id.

        inner(n+1)
      }
    }

    inner(1) // start at 1
  }

}
