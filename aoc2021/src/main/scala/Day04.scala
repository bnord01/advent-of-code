import Utils.seqFromFileLines

import scala.util.control.Breaks._

object Day04 extends App {
  val input = seqFromFileLines("day04.txt")
  val moves = input.head.split(",").map(_.toInt)
  val boards = input.tail.grouped(6).map {
    _.tail
  }.map {
    _.map { l => l.grouped(3).map(_.trim.toInt).toArray }.toArray
  }.toArray
  var hits = Array.ofDim[Boolean](boards.length, 5, 5)
  var hits_trans = Array.ofDim[Boolean](boards.length, 5, 5)

  def wins(b: Int): Boolean = {
    hits(b).exists(l => l.forall(v => v)) || hits_trans(b).exists(l => l.forall(v => v))
  }

  def play(b: Int, z: Int): Unit = {
    for (i <- boards(b).indices; j <- boards(b)(i).indices; if boards(b)(i)(j) == z) {
      hits(b)(i)(j) = true
      hits_trans(b)(j)(i) = true
    }
  }

  def value(b: Int, z: Int): Int = {
    (for (i <- boards(b).indices; j <- boards(b)(i).indices; if !hits(b)(i)(j)) yield {
      boards(b)(i)(j)
    }).sum * z
  }

  breakable {
    for (z <- moves; b <- boards.indices) {
      play(b, z)
      if (wins(b)) {
        println(value(b, z))
        break()
      }
    }
  }

  // Part 2
  hits = Array.ofDim[Boolean](boards.length, 5, 5)
  hits_trans = Array.ofDim[Boolean](boards.length, 5, 5)

  boards.indices.flatMap { b =>
    moves.zipWithIndex.find { case (z, _) =>
      play(b, z); wins(b)
    } map { case (z, i) => (value(b, z), i) }
  }.maxBy(_._2) match {
    case (v, _) => println(v)
  }
}