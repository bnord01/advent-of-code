import Utils.seqFromFileLines


object Day01 extends App {
  val input = seqFromFileLines("day01.txt")
  val depth = input.map(_.trim.toInt)

  // Part 1
  println(depth.sliding(2).count { case Seq(x,y) => x < y })

  // Part 2
  println(
    depth
      .sliding(3)
      .flatMap {
        case Seq(a,b,c) => Some(a+b+c)
        case _ => None
      }
      .sliding(2)
      .count {
        case Seq(x,y) => x < y
      }
  )
}