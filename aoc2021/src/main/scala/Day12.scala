import Utils.seqFromFileLines

object Day12 extends App {
  val input = seqFromFileLines("day12.txt")
  val edges = input.map(_.split("-") match { case Array(a, b) => (a, b) })
  type Node = String
  val successors =
    edges.foldLeft(Map[Node, Set[Node]]().withDefaultValue(Set())) {
      case (succs, (a, b)) => succs + (a -> (succs(a) + b)) + (b -> (succs(b) + a))
    }

  def lower(s: Node) = !s.charAt(0).isUpper

  val start = "start"
  val end = "end"

  // Part 1

  def dfs_one(n: Node, seen: Set[Node] = Set()): Set[List[Node]] = {
    if (seen(n))
      Set()
    else if (n == end)
      Set(n :: Nil)
    else {
      val new_seen = if (lower(n)) seen + n else seen
      successors(n).flatMap(dfs_one(_, new_seen)).map(n :: _)
    }
  }

  println(dfs_one(start).size)


  // Part 2

  def dfs_two(n: Node, seen: Set[Node] = Set(), bonus: Boolean = true): Set[List[Node]] = {
    if (seen(n)) {
      if (!bonus || n == start || n == end)
        Set()
      else
        successors(n).flatMap(dfs_two(_, seen, bonus = false)).map(n :: _)
    }
    else if (n == end)
      Set(n :: Nil)
    else {
      val new_seen = if (lower(n)) seen + n else seen
      successors(n).flatMap(dfs_two(_, new_seen, bonus)).map(n :: _)
    }
  }

  println(dfs_two(start).size)
}