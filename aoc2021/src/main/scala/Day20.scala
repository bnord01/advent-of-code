import Utils.seqFromFileLines

object Day20 extends App {
  val input = seqFromFileLines("day20test.txt")

  val rules = input.head.toCharArray.map { case '#' => '1'; case '.' => '0' }

  implicit class WithToIntBin(val s: String) extends AnyVal {
    def toIntBin: Int = Integer.parseInt(s, 2)
  }

  def step(in: Array[Array[Char]], pad: Char = '0'): Array[Array[Char]] = {
    val padded = Array.fill[Char](in.length + 4, in(0).length + 4)(pad)
    for (i <- in.indices) {
      Array.copy(in(i), 0, padded(i + 2), 2, in(i).length)
    }
    Array.tabulate[Char](in.length + 2, in(0).length + 2)((i, j) =>
      rules(Array.concat[Char]((i until i + 3).map(padded(_).slice(j, j + 3)): _*).mkString("").toIntBin))
  }

  def pad(round:Int): Char = if(rules(0) == '0' || round % 2 == 0) '0' else '1'

  val initial_board = input.tail.tail.toArray.map(_.toCharArray.map { case '#' => '1'; case '.' => '0' })

  // Part 1
  val result1 = (0 until 2).foldLeft(initial_board)((b, i) => step(b, pad(i)))
  println(result1.map(_.count(_ == '1')).sum)

  // Part 2
  val result2 = (2 until 50).foldLeft(result1)((b, i) => step(b, pad(i)))
  println(result2.map(_.count(_ == '1')).sum)
}