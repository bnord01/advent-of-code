import Utils.seqFromFileLines
import breeze.linalg.{DenseMatrix, DenseVector, sum}
import breeze.math.Ring.ringFromField
import breeze.numerics.abs


object Day19 extends App {
  val input = seqFromFileLines("day19.txt")
  val reports = ScannerParser.parseScanners(input)

  type Report = Set[DenseVector[Int]]

  val rotations: Set[DenseMatrix[Int]] = {
    val rotX = DenseMatrix((1, 0, 0), (0, 0, -1), (0, 1, 0))
    val rotY = DenseMatrix((0, 0, 1), (0, 1, 0), (-1, 0, 0))
    val rotZ = DenseMatrix((0, -1, 0), (1, 0, 0), (0, 0, 1))
    (
      for (x <- 0 to 3; y <- 0 to 3; z <- 0 to 3) yield {
        var r = DenseMatrix.eye[Int](3)
        (0 until x).foreach { _ => r = r * rotX }
        (0 until y).foreach { _ => r = r * rotY }
        (0 until z).foreach { _ => r = r * rotZ }
        r
      }).toSet
  }

  def align(r1: Report, r2: Report): Option[(Report, DenseVector[Int])] = {
    for (r2r <- rotations.map { rot => r2.map { rot * _ } }) {
      for (b2 <- r2r) {
        for (b1 <- r1) {
          val r2t = r2r.map(_ + (b1 - b2))
          if (r2t.intersect(r1).sizeIs > 4)
            return Some((r2t, b1 - b2))
        }
      }
    }
    None
  }

  var scanners = Set(DenseVector(0, 0, 0))
  var aligned = reports(0)
  var to_align = reports.tail

  while (to_align.nonEmpty) {
    val rao = align(aligned, to_align.head)
    if (rao.nonEmpty) {
      to_align = to_align.tail
      aligned ++= rao.get._1
      scanners += rao.get._2
    } else {
      to_align = to_align.tail.appended(to_align.head)
    }
  }

  // Part 1
  println(aligned.size)

  // Part 2
  println((for (s1 <- scanners; s2 <- scanners) yield sum(abs(s1 - s2))).max)
}


object ScannerParser {
  def parseScanners(input: Seq[String]): Array[Set[DenseVector[Int]]] = {
    var scanners = Map[Int, Set[DenseVector[Int]]]().withDefaultValue(Set())
    var c = 0
    for (line <- input) {
      if (line == "")
        c += 1
      else if (!line.startsWith("---")) {
        line.split(",").map(_.toInt) match {
          case Array(x, y, z) =>
            scanners += c -> (scanners(c) + DenseVector(x, y, z))
        }
      }
    }
    (0 to c).toArray.map(scanners)
  }
}