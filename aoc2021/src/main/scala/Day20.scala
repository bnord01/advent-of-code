object Day20 extends App {
  val input = Utils.seqFromFileLines("day20.txt")
  val rules = input.head.toCharArray.map { case '#' => 1; case '.' => 0 }
  val initial_board = input.tail.tail.toArray.map(_.toCharArray.map { case '#' => 1; case '.' => 0 })

  def step(in: Array[Array[Int]], pad: Int = '0'): Array[Array[Int]] = {
    val padded = Array.fill[Int](in.length + 4, in(0).length + 4)(pad)
    for (i <- in.indices) {
      Array.copy(in(i), 0, padded(i + 2), 2, in(i).length)
    }
    Array.tabulate[Int](in.length + 2, in(0).length + 2)((i, j) =>
      rules((0 to 8).foldLeft(0)((x, l) => x | padded(i + l / 3)(j + l % 3) << (8 - l)))
    )
  }

  def pad(round: Int): Int = if (rules(0) == 0 || round % 2 == 0) 0 else 1

  // Part 1
  val result1 = (0 until 2).foldLeft(initial_board)((b, i) => step(b, pad(i)))
  println(result1.map(_.sum).sum)

  // Part 2
  val result2 = (2 until 50).foldLeft(result1)((b, i) => step(b, pad(i)))
  println(result2.map(_.sum).sum)
}