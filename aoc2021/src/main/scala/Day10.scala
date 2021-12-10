import Utils.seqFromFileLines

object Day10 extends App {
  val input = seqFromFileLines("day10.txt")
  val opening = Set('<', '(', '[', '{')
  val matching = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  // Part 1
  val value1 = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  input.map { line =>
    var stack: List[Char] = Nil
    def score(line: String): Int = {
      for (c <- line) {
        if (opening(c))
          stack = matching(c) :: stack
        else if (stack.isEmpty || stack.head != c)
          return value1(c)
        else
          stack = stack.tail
      }
      0
    }
    score(line)
  }.sum match {
    case v => println(v)
  }

  // Part 2
  val value2 = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
  input.flatMap { line =>
    var stack: List[Char] = Nil
    def score(line: String): Option[Long] = {
      for (c <- line) {
        if (opening(c))
          stack = matching(c) :: stack
        else if (stack.isEmpty || stack.head != c)
          return None
        else
          stack = stack.tail
      }
      if (stack.isEmpty)
        None
      else
        Some(stack.foldLeft(0L) { case (v, c) => 5 * v + value2(c) })
    }
    score(line)
  }.sorted match {
    case scores => println(scores(scores.length / 2))
  }
}