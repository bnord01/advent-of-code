import scala.collection.mutable

object Day24 extends App {
  val blocks = Utils.seqFromFileLines("day24.txt").grouped(18)
    .map(Block.parse).zipWithIndex.map(x => x._2 -> x._1).toMap

  def find(max:Boolean): String = {
    val ws = if (max) 9.to(1,-1) else 1 to 9
    // Basically Dijkstra again
    val seen = mutable.Set[(Int,Int)]()
    def find_helper(idx:Int, z:Int):Option[String] = {
      if (idx == 14 && z == 0) Some("")
      else if (idx == 14) None
      else if (seen(idx,z)) None else {
        seen += ((idx,z))
        for (w <- ws; r <- find_helper(idx+1,blocks(idx)(w,z)))
          return Some(w + r)
        None
      }
    }
    find_helper(0,0).get
  }

  println(find(max=true))
  println(find(max=false))

}

case class Block(zdiv:Int, xadd:Int, yadd:Int, yadd2:Int) {
  def apply(w0:Int, z0:Int = 0): Int = {
    val w = w0
    var x = z0 % 26
    var z = z0 / zdiv
    x += xadd
    x = if (x == w) 0 else 1
    var y = yadd
    y = x * y
    y += 1
    z = z*y
    y = w + yadd2
    y = y * x
    z += y
    z
  }
}

object Block {
  def parse(lines:Seq[String]): Block = {
    assert(lines.length == 18)
    assert(lines.startsWith(List("inp w", "mul x 0", "add x z", "mod x 26")))
    assert(lines(4).startsWith("div z "))
    val zdiv = lines(4).drop(6).toInt
    assert(lines(5).startsWith("add x "))
    val xadd = lines(5).drop(6).toInt
    assert(lines(6) == "eql x w")
    assert(lines(7) == "eql x 0")
    assert(lines(8) == "mul y 0")
    assert(lines(9).startsWith("add y "))
    val yadd = lines(9).drop(6).toInt
    assert(lines(10) == "mul y x")
    assert(lines(11) == "add y 1")
    assert(lines(12) == "mul z y")
    assert(lines(13) == "mul y 0")
    assert(lines(14) == "add y w")
    assert(lines(15).startsWith("add y "))
    val yadd2 = lines(15).drop(6).toInt
    assert(lines(16) == "mul y x")
    assert(lines(17) == "add z y")
    Block(zdiv,xadd,yadd,yadd2)
  }
}
