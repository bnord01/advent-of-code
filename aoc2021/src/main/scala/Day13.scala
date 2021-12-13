import Utils.seqFromFileLines

sealed trait Fold

case class X(column: Int) extends Fold

case class Y(line: Int) extends Fold

case class Cross(x: Int, y: Int) {
  def swap: Cross = Cross(y, x)
}

object Day13 extends App {
  val input = seqFromFileLines("day13.txt")

  val crosses = input
    .takeWhile(x => !x.isBlank)
    .map {
      _.split(",") match {
        case Array(a, b) => Cross(a.toInt, b.toInt)
      }
    }.toSet

  val folds: Seq[Fold] = input
    .dropWhile(x => !x.isBlank)
    .drop(1)
    .map {
      _.split("=") match {
        case Array("fold along x", b) => X(b.toInt)
        case Array("fold along y", b) => Y(b.toInt)
      }
    }

  def foldX(cross: Cross, x: Int): Option[Cross] = {
    if (x > cross.x)
      Some(cross)
    else if (x == cross.x)
      None
    else
      Some(Cross(2 * x - cross.x, cross.y))
  }

  def fold(cross: Cross, f: Fold): Option[Cross] = f match {
    case X(x) => foldX(cross, x)
    case Y(y) => foldX(cross.swap, y).map(_.swap)
  }

  // Part 1
  println(crosses.map(fold(_, folds.head)).size)

  // Part 2
  val final_crosses = folds.foldLeft(crosses) { case (cs, f) => cs.flatMap(fold(_, f)) }

  val field = Array.ofDim[String](final_crosses.map(_.y).max + 1, final_crosses.map(_.x).max + 1)
  for (y <- field.indices; x <- field(y).indices) {
    field(y)(x) = if (final_crosses(Cross(x, y))) "X" else " "
  }

  println(field.map(_.mkString("")).mkString("\n"))

}