import Utils.seqFromFileLines

object Day09 extends App {
  val input = seqFromFileLines("day09.txt")
  val field = input.map(_.split("").map(_.toInt)).toArray
  val NX = field.length
  val NY = field.head.length

  def neighboursX(i: Int, j: Int) = if (i == 0) Seq((i + 1, j)) else if (i >= NX - 1) Seq((i - 1, j)) else Seq((i - 1, j), (i + 1, j))

  def neighboursY(i: Int, j: Int) = if (j == 0) Seq((i, j + 1)) else if (j >= NY - 1) Seq((i, j - 1)) else Seq((i, j - 1), (i, j + 1))

  def neighbours(i: Int, j: Int) = neighboursX(i, j) ++ neighboursY(i, j)

  val minima = for (i <- 0 until NX;
                    j <- 0 until NY;
                    if neighbours(i, j).forall { case (a, b) => field(i)(j) < field(a)(b) }
                    ) yield {
    (i, j)
  }

  // Part 1
  println(minima.map { case (a, b) => field(a)(b) + 1 }.sum)

  // Part 2

  // Helper function
  def fixpoint[A](a: A)(f: A => A): A = {
    var last: Option[A] = None;
    var current = a;
    while (!last.contains(current)) {
      last = Some(current)
      current = f(last.get)
    }
    current
  }

  val valleys = minima.map {
    m =>
      fixpoint(Set(m)) {
        x =>
          x ++ x.flatMap {
            case (i, j) =>
              neighbours(i, j).filter { case (a, b) => field(a)(b) < 9 && field(a)(b) >= field(i)(j) }
          }
      }
  }.map(_.size).sorted

  println(valleys.takeRight(3).product)
}