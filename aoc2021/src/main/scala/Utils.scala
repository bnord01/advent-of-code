import scala.io.Source

object Utils {
  def seqFromFileLines(fileName: String): Seq[String] =
    Source.fromResource(fileName).getLines().toSeq
}
