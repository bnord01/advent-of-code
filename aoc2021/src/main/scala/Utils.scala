import scala.io.Source

object Utils {
  def seqFromFileLines(fileName: String): Seq[String] =
    Source.fromResource(fileName).getLines().toSeq

  def time[R](body: => R): R = {
    val start = System.currentTimeMillis()
    val result = body
    val end = System.currentTimeMillis()
    println(f"Elapsed time: ${end - start}ms")
    result
  }
}
