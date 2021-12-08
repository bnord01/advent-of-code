import Utils.seqFromFileLines

object Day02 extends App {
  val input = seqFromFileLines("day02.txt")
  val moves = input.map(l => l.split(" ") match {
    case Array(x: String, y: String) => (x, y.toInt)
  })

  // Part 1
  val forward = moves.collect { case ("forward", x) => x }
  val down = moves.collect { case ("down", x) => x }
  val up = moves.collect { case ("up", x) => x }

  println(forward.sum * (down.sum - up.sum))

  // Part 2
  moves.foldLeft((0, 0, 0)) {
    case ((depth, horizontal, aim), (move, dist)) => move match {
      case "forward" => (depth + (dist * aim), horizontal + dist, aim)
      case "up" => (depth, horizontal, aim - dist)
      case "down" => (depth, horizontal, aim + dist)
    }
  } match {
    case (d, h, _) => println(d * h)
  }
}