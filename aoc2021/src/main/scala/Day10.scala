import Utils.seqFromFileLines

object Day10 extends App {
  val input = seqFromFileLines("day10.txt")
  val opening = Set('<', '(', '[', '{')
  val matching = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  // Part 1
  val valueOne = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

  def scoreOne(line: String): Int = {
    var stack: List[Char] = Nil
    for (c <- line) {
      if (opening(c))
        stack = matching(c) :: stack
      else if (stack.isEmpty || stack.head != c)
        return valueOne(c)
      else
        stack = stack.tail
    }
    0
  }

  println(input.map {
    scoreOne
  }.sum)


  // Part 2
  val valueTwo = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  def scoreTwo(line: String): Option[Long] = {
    var stack: List[Char] = Nil
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
      Some(stack.foldLeft(0L) { case (v, c) => 5 * v + valueTwo(c) })
  }

  input.flatMap { scoreTwo }.sorted match {
    case scores => println(scores(scores.length / 2))
  }
}