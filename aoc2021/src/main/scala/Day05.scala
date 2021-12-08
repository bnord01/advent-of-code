import math.{min, max, signum, abs}

object Day05 extends App {

  val input = Utils.seqFromFileLines("day05.txt")
  val lines = input.map(_.split(",|(->)").map(_.trim.toInt))
  val orthlines = lines.filter(orth).map(orderLine)
  val diags = lines.filterNot(orth)

  val maxx = orthlines.flatMap(l => Seq(l(0), l(2))).max
  val maxy = orthlines.flatMap(l => Seq(l(1), l(3))).max
  var critcount = 0
  val ar = for (y <- Array.from(0 to maxy)) yield for (x <- Array.from(0 to maxx)) yield {
    var count = 0
    for (line <- orthlines if orth(line) && onOrthLine(x, y, line(0), line(1), line(2), line(3)))
      count += 1
    for (line <- diags) {
      val dx = line(2) - line(0)
      val dy = line(3) - line(1)
      val sx = x - line(0)
      val sy = y - line(1)
      if ((sx == 0 && sy == 0) || abs(sx) == abs(sy) && signum(sx) == signum(dx) && signum(sy) == signum(dy) && abs(sx) <= abs(dx) && abs(sy) <= abs(dy))
        count += 1
    }
    if (count > 1) {
      critcount += 1
    }
    count
  }
  println(critcount)
  //println(ar.map(_.mkString("")).mkString("\n"))

  def onOrthLine(x: Int, y: Int, sx: Int, sy: Int, tx: Int, ty: Int) = sx <= x && x <= tx && sy <= y && y <= ty

  def orth(line: Array[Int]) = line(0) == line(2) || line(1) == line(3)

  def orderLine(l: Array[Int]) = Array(min(l(0), l(2)), min(l(1), l(3)), max(l(0), l(2)), max(l(1), l(3)))

}