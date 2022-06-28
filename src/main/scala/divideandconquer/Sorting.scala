package divideandconquer

import java.util.Scanner
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Sorting {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val seq = new Array[Long](n)
    for (i <- 0 until n) seq(i) = s.nextLong()
    quickSort(seq).foreach(x => print(s"$x "))
  }

  def quickSort(seq: Array[Long]): Array[Long] = {

    val (s1, s2, s3) = partition3(seq)
    val q1 = if (s1.length > 1) quickSort(s1) else s1 // arrays of one element are done sorting
    val q3 = if (s3.length > 1) quickSort(s3) else s3
    Array.concat(q1, s2, q3)
  }

  def partition3(seq: Array[Long]): (Array[Long], Array[Long], Array[Long]) = {

    val pv = seq(Random.nextInt(seq.length))
    val less = new ArrayBuffer[Long]
    val equal = new ArrayBuffer[Long]
    val more = new ArrayBuffer[Long]

    seq.foreach(l => {
      if (l < pv) less.append(l)
      else if (l > pv) more.append(l)
      else equal.append(l)
    })

    (less.toArray, equal.toArray, more.toArray)
  }

}
