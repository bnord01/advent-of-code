import math.abs
import Utils.seqFromFileLines

object Day07 extends App {
  val input = seqFromFileLines("day07.txt")
  val crabs = Array.from(input.flatMap(_.split(',')).map(_.toInt))
  crabs.sortInPlace()
  val median = crabs(crabs.length / 2)
  println(median)
  println(crabs.map(x => abs(x - median)).sum)

  val avg1 = crabs.sum / crabs.length
  println(avg1)
  println(crabs.map(x => abs(x - avg1)).map(dist => dist * (dist + 1) / 2).sum)

  val avg2 = avg1 + 1
  println(avg2)
  println(crabs.map(x => abs(x - avg2)).map(dist => dist * (dist + 1) / 2).sum)

  println((1 to crabs.length).map(point => crabs.map(x => abs(x - point)).map(dist => dist * (dist + 1) / 2).sum).min)
}