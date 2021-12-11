import Utils.seqFromFileLines

import scala.collection.mutable

object Day11 extends App {
  val input = seqFromFileLines("day11.txt")

  var octopuses = input.map(_.split("").map(_.toInt)).toArray
  val n = octopuses.length
  val m = octopuses.head.length

  def neighbors(i: Int, j: Int): Seq[(Int, Int)] =
    for (a <- i - 1 to i + 1; b <- j - 1 to j + 1 if 0 <= a && a < n &&
      0 <= b && b < m && !(i == a && j == b)) yield (a, b)


  def step: Int = {
    var count = 0
    val q = mutable.Queue.empty[(Int, Int)]
    for (i <- octopuses.indices; j <- octopuses.head.indices) {
      octopuses(i)(j) += 1
      if (octopuses(i)(j) == 10) {
        q.append((i, j))
        count += 1
      }
    }
    while (q.nonEmpty) {
      val (i, j) = q.dequeue()
      for ((a, b) <- neighbors(i, j)) {
        octopuses(a)(b) += 1
        if (octopuses(a)(b) == 10) {
          q.append((a, b))
          count += 1
        }
      }
    }
    for (i <- octopuses.indices; j <- octopuses.head.indices) {
      if (octopuses(i)(j) >= 10) {
        octopuses(i)(j) = 0
      }
    }
    count
  }

  // Part 1
  println((1 to 100 map { _ => step }).sum)

  // Part 2
  octopuses = input.map(_.split("").map(_.toInt)).toArray
  println(LazyList.from(1).find(_ => step == 100).get)
}