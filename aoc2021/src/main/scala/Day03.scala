import Utils.seqFromFileLines

object Day03 extends App {
  val input = seqFromFileLines("day03.txt")
  val bits = input.map { l => l.split("").map(_.toInt) }

  // Part 1
  val counts = Array.ofDim[Int](bits.head.length)
  bits.foreach(_.zipWithIndex.foreach { case (bit, i) => counts(i) += bit })
  val threshold = bits.length / 2

  val gamma = counts.foldRight((0, 1)) { case (i, (acc, f)) => (if (i >= threshold) acc + f else acc, f * 2) }._1
  val epsilon = counts.foldRight((0, 1)) { case (i, (acc, f)) => (if (i < threshold) acc + f else acc, f * 2) }._1
  println(gamma * epsilon)

  // Part 2
  // Oxygen Generator Rating
  var ogbits = bits
  var idx = 0
  while (ogbits.sizeIs > 1) {
    val ones = ogbits.map(_ (idx)).sum
    val zeros = ogbits.length - ones
    if (ones >= zeros)
      ogbits = ogbits.filter(_ (idx) == 1)
    else
      ogbits = ogbits.filter(_ (idx) == 0)
    idx += 1
  }
  val ograting = ogbits.head.foldRight((0, 1)) { case (i, (acc, f)) => (acc + i * f, f * 2) }._1

  // CO2 Rating
  var cobits = bits
  idx = 0
  while (cobits.sizeIs > 1) {
    val ones = cobits.map(_ (idx)).sum
    val zeros = cobits.length - ones
    if (zeros <= ones)
      cobits = cobits.filter(_ (idx) == 0)
    else
      cobits = cobits.filter(_ (idx) == 1)
    idx += 1
  }
  val corating = cobits.head.foldRight((0, 1)) { case (i, (acc, f)) => (acc + i * f, f * 2) }._1

  // Result
  println(corating * ograting)
}