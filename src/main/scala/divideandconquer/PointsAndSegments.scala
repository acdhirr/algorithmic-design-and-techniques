package divideandconquer

import java.util.Scanner
import scala.annotation.tailrec

object PointsAndSegments {

  case class Segment(start: Int, end: Int) {
    def covers(t: Int): Boolean = start <= t && t <= end
    override def toString() = s"[$start - $end]"
  }

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val ns = s.nextInt()
    val np = s.nextInt()
    val segments = new Array[Segment](ns)
    for (i <- 0 until ns) segments(i) = Segment(s.nextInt(), s.nextInt())
    val points = new Array[Int](np)
    for (i <- 0 until np) points(i) = s.nextInt()

    /*
    8 1
    -1 1
    0 3
    5 6
    4 10
    -2 7
    -2 3
    -10 4
    6 17
    4

    point = 4  -> [4,10] [-2,7] [-10,4]
    answer: 3

    1 1
    2 2
    2
    answer: 1

    4 1
    -10 0
    -5 0
    1 3
    2 4
    0
    answer: 2

    */

    val endSortedSegments = segments.toList.sortBy(_.end).toArray
    val startSortedSegments = segments.toList.sortBy(_.start).toArray

    /* Strategy:
     pE = index in endSortedSegments of segments that end BEFORE our point
     pS = index in startSortedSegments of segments that start AFTER our point
     Then:
      ns - (ns - pS) - pE
      or pS - pE
      segments are covering our point
    */

    val r = points.map(point => {
      val (pE,pS) = countPointInSegments(point, endSortedSegments, startSortedSegments)
      pS - pE
    })

    print(r.mkString(" "))
  }

  def countPointInSegments(point: Int, endSortedSegments: Array[Segment], startSortedSegments: Array[Segment]): (Int, Int) = {

    // count all segments that end before point, they must be discarded
    val tooFarLeftIndex = binarySearch(point, endSortedSegments,true)
    // count all segments that start after point, they must be discarded too
    val tooFarRightIndex = binarySearch(point, startSortedSegments,false)

    // println()
    // println(s"$tooFarLeftIndex<- p=$point ->$tooFarRightIndex")
    (tooFarLeftIndex, tooFarRightIndex)
  }

  def binarySearch(q: Long, values: Array[Segment], useEnd: Boolean): Int = {

    @tailrec
    def _binarySearch(q: Long, from: Int, to: Int, values: Array[Segment]): Int = {

      if (from > to) from // exhausted, return insertion point
      else {
        val mid = (from + to) / 2
        val usePoint = if (useEnd) values(mid).end else values(mid).start-1 // including start!
        if (usePoint < q)
          _binarySearch(q, mid + 1, to, values)
        else
          _binarySearch(q, from, mid - 1, values)
      }
    }

    _binarySearch(q, 0, values.length-1, values)
  }

}
