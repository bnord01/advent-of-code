object Day06 extends App {
  val input = Utils.seqFromFileLines("day06.txt")
  val fish = input.flatMap(_.split(',')).map(_.toInt);
  var counts = Array.ofDim[Long](9);
  val ages = Array.from(0 to 8)
  for (f <- fish) {
    counts(f) += 1
  }
  for (_ <- 0 until 256) {
    counts = ages.map {
      case 8 => counts(0)
      case 6 => counts(0) + counts(7)
      case i => counts(i + 1)
    }
  }
  println(counts.sum)
}