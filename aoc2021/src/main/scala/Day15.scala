import Utils.seqFromFileLines

import scala.collection.mutable

import scala.math.Ordering.Implicits._

object Day15 extends App {
  val input = seqFromFileLines("day15.txt")

  type Node = (Int, Int)

  def dijkstra(start: Node, end: Node, neighbours: Node => IterableOnce[Node], weight: Node => Int): Long = {
    val minDist = mutable.Map[Node, Long]().withDefaultValue(Long.MaxValue)

    val nearer = new Ordering[Node] {
      def compare(a: Node, b: Node): Int = {
        -seqOrdering[Seq, Long].compare(Seq(minDist(a), a._1, a._2), Seq(minDist(b), b._1, b._2))
      }
    }

    val q = mutable.PriorityQueue.empty(nearer)

    minDist += start -> 0
    q += start

    var not_found = true
    while (q.nonEmpty && not_found) {
      val next = q.dequeue()
      if (next == end)
        not_found = false
      else {
        val dist = minDist(next)
        for (n <- neighbours(next)) {
          val ndist = dist + weight(n)
          // Not really Dijkstra, as all edges leading to a node have the same weight first visit is shortest.
          if (!minDist.keySet(n)) {
            minDist += n -> ndist
            q += n
          }
        }
      }
    }
    minDist(end)
  }

  // Part 1
  val field = input.map(_.split("").map(_.toInt)).toArray

  val NX = field.length - 1
  val NY = field.head.length - 1

  def neighboursX(i: Int, j: Int) = if (i == 0) Seq((i + 1, j)) else if (i >= NX) Seq((i - 1, j)) else Seq((i - 1, j), (i + 1, j))

  def neighboursY(i: Int, j: Int) = if (j == 0) Seq((i, j + 1)) else if (j >= NY) Seq((i, j - 1)) else Seq((i, j - 1), (i, j + 1))

  def neighbours(p: Node) = p match {
    case (i, j) => neighboursX(i, j) ++ neighboursY(i, j)
  }

  println(dijkstra((0, 0), (NX, NY), neighbours, { case (a, b) => field(a)(b) }))


  // Part 2
  def weight2(n: Node) = n match {
    case (i, j) =>
      val i0 = i % (NX + 1)
      val j0 = j % (NY + 1)
      val orig_weigth = field(i0)(j0)
      val repx = i / (NX + 1)
      val repy = j / (NY + 1)
      (orig_weigth + repx + repy + - 1) % 9 + 1
  }

  val NX2 = field.length * 5 - 1
  val NY2 = field.head.length * 5 - 1

  def neighbours2X(i: Int, j: Int) = if (i == 0) Seq((i + 1, j)) else if (i >= NX2) Seq((i - 1, j)) else Seq((i - 1, j), (i + 1, j))

  def neighbours2Y(i: Int, j: Int) = if (j == 0) Seq((i, j + 1)) else if (j >= NY2) Seq((i, j - 1)) else Seq((i, j - 1), (i, j + 1))

  def neighbours2(p: Node) = p match {
    case (i, j) => neighbours2X(i, j) ++ neighbours2Y(i, j)
  }

  println(dijkstra((0, 0), (NX2, NY2), neighbours2, weight2))
}