package greedy

import java.util.Scanner
import scala.collection.mutable.ListBuffer

object CoveringSegments {

  case class Segment(start: Int, end: Int) extends Ordered[Segment] {
    def covers(t: Int): Boolean = start <= t && t <= end

    override def compare(that: Segment): Int = this.end - that.end

    override def toString() = s"[$start - $end]"
  }

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val segments = new Array[Segment](n)
    for (i <- 0 until n) segments(i) = Segment(s.nextInt(), s.nextInt())
    // segments sorted by their end coordinate
    val endSortedSegments = segments.toList.sorted

    val points = optimalPoints(endSortedSegments, List[Int]()).sorted

    println(points.length)
    points.foreach(p => print(s"$p "))
  }

  def optimalPoints(segments: List[Segment], transport: List[Int]): List[Int] = {

    //segments.foreach(s => println(s))

    segments match {
      case Nil => transport
      case head :: tail =>
        // pick first segment
        val point = head.end
        // remove all segments who's start is before point
        optimalPoints(cleanSegmentsBefore(tail, point), point :: transport)
    }

  }

  /*
    All segments having a start before a found end point p will be dismissed.
    They contain p, since their end point must be after p because of the ordering by end point.
  */
  def cleanSegmentsBefore(segments: List[Segment], point: Int): List[Segment] = {

    val cSegments = new ListBuffer[Segment]()
    for (s <- segments) if (s.start > point) cSegments += s
    cSegments.toList
  }

}
