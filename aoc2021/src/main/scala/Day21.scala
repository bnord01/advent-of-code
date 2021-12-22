import scala.collection.mutable

case class Player(var pos: Int, var score: Int = 0) {
  pos = pos - 1

  def move_and_check_win(n: Int): Boolean = {
    pos = (pos + n) % 10
    score += pos + 1
    score >= 1000
  }
}

object Day21 extends App {

  // Part 1

  // val p1 = Player(4); val p2 = Player(8) // Example
  val p1 = Player(9); val p2 = Player(3) // Real
  val players = LazyList.continually(Seq(p1, p2)).flatten
  val dice = LazyList.from(0).map(i => 3 * (2 + 3 * i))

  val win_other_turn = ((players lazyZip dice) map ((p, d) => p.move_and_check_win(d)) lazyZip players.tail lazyZip LazyList.from(1)).find(_._1)
  win_other_turn match {
    case Some((_, p, t)) => println(f"Turn: $t, Score: ${p.score}, Result: ${p.score * t * 3}")
  }

  // Part 2

  val rolls = Array.from(for (i1 <- 1 to 3; i2 <- 1 to 3; i3 <- 1 to 3) yield i1 + i2 + i3)

  val win_table: mutable.Map[(Int, Int, Int, Int), (Long, Long)] = mutable.Map.empty

  def wins(s1: Int, s2: Int, p1: Int = 0, p2: Int = 0): (Long, Long) = win_table.getOrElseUpdate((s1, s2, p1, p2), {
    if (p1 > 20) (1, 0)
    else if (p2 > 20) (0, 1)
    else rolls.map(d => wins(s2, (s1 + d) % 10, p2, p1 + 1 + ((s1 + d) % 10))).foldLeft((0L, 0L)) { case ((x1, x2), (y2, y1)) => (x1 + y1, x2 + y2) }
  })

  // println(wins(4-1,8-1)) // Test
  println(wins(9 - 1, 3 - 1) match { case (a, b) => math.max(a, b) }) // Real
}
