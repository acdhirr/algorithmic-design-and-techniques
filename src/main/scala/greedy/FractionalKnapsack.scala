package greedy

import java.util.Scanner
import scala.annotation.tailrec

object FractionalKnapsack {

  case class Compound(value: Double, weight: Double) extends Ordered[Compound] {

    lazy val unitPrice: Double = value / weight

    def take(weight: Double): Compound = Compound(weight * unitPrice, weight)

    // Compounds wil be sorted by unitPrice
    override def compare(that: Compound): Int =
      (that.value * this.weight - this.value * that.weight).toInt

    override def toString = s"($value, $weight) = $unitPrice"
  }

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val capacity = s.nextInt()
    val compounds = new Array[Compound](n)

    for (i <- 0 until n)
      compounds(i) = Compound(s.nextInt(), s.nextInt())

    // Feed sorted compounds to function!
    val opt = getOptimalValue(capacity, compounds.toList.sorted, 0)
    println(f"$opt%1.4f") // format with 4 digits after decimal point
  }

  @tailrec
  def getOptimalValue(capacity: Double, compounds: List[Compound], transport: Double): Double = {

    compounds match {
      case Nil => transport
      case h :: tail if h.weight <= capacity => getOptimalValue(capacity - h.weight, tail, transport + h.value)
      case h :: _ => transport + h.take(capacity).value // take just as much as fits in the remaining space
    }
  }

}
