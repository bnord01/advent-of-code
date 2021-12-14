import Utils.seqFromFileLines

import scala.collection.mutable


object Day14 extends App {
  val input = seqFromFileLines("day14.txt")

  val sequence = input.head

  val templates: Map[String, String] = Map.from(input.drop(2).map {
    _.split(" -> ") match {
      case Array(a, b) => a -> b
    }
  })

  // Part 1
  val templates2 = templates.map { case (a, b) => a -> (b + a.substring(1,1)) }

  var nseq = sequence
  val s0 = sequence.substring(0, 1)
  for (_ <- 1 to 10)
    nseq = nseq.sliding(2).map(templates2).mkString(start = s0, "", "")

  val counts1: Set[Int] = nseq.toSet.map { (c: Char) => nseq.count { c1 => c1 == c } }

  println(counts1.max - counts1.min)

  // Part 2

  def sumMap[K](ms: Iterable[Map[K, Long]]): Map[K, Long] = ms.reduce[Map[K, Long]] {
    case (m1, m2) => Map.from((m1.keys ++ m2.keys).map { x => x -> (m1.getOrElse(x, 0L) + m2.getOrElse(x, 0L)) })
  }

  val successors = templates.map { case (a, b) => a -> Seq(a.substring(0,1) + b, b + a.substring(1,1)) }

  val mem = mutable.Map[(String, Int), Map[String, Long]]()

  def count(s: String, steps: Int): Map[String, Long] = mem.getOrElseUpdate((s, steps), {
    if (steps == 0)
      Map[String, Long]()
    else
      sumMap(successors(s).map(count(_, steps - 1)) :+ Map(templates(s) -> 1L))
  })

  val counts2 = sumMap(
    sequence.sliding(2).map { s => count(s, 40) } ++: sequence.split("").map { c => Map(c -> 1L) }
  ).values
  println(counts2.max - counts2.min)
}