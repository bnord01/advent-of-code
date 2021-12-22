import Utils.seqFromFileLines
import org.parboiled2.{CharPredicate, Parser, ParserInput, Rule1}

object Day22 extends App {

  val input = seqFromFileLines("day22.txt")

  // Part 1
  val actions1 = input.map(Action.parse(_, 50))
  val cubes = Array.ofDim[Int](50 * 2 + 1, 50 * 2 + 1, 50 * 2 + 1)
  for (Action(cmd, xs, ys, zs) <- actions1; x <- xs; y <- ys; z <- zs) {
    cubes(x + 50)(y + 50)(z + 50) = cmd
  }
  println(cubes.map(_.map(_.sum).sum).sum)

  // Part 2
  def count_on(actions: Seq[Action]): Long = {
    actions.zipWithIndex.foldLeft(0L) { case (sum, (action, num)) =>
      val on_inside = count_on(actions.take(num).flatMap(action.intersect))
      if (action.on) sum + action.size - on_inside
      else sum - on_inside
    }
  }

  println(count_on(input.map(Action.parse(_))))
}

case class Action(cmd: Int, x: Range, y: Range, z: Range) {
  def intersect(o: Action): Option[Action] = {
    val a = Action(o.cmd, interRange(x, o.x), interRange(y, o.y), interRange(z, o.z))
    if (a.nonEmpty) Some(a) else None
  }

  def nonEmpty: Boolean = x.nonEmpty && y.nonEmpty && z.nonEmpty

  def size: Long = x.size.toLong * y.size * z.size

  def on: Boolean = cmd == 1

  private def interRange(r1: Range, r2: Range): Range =
    math.max(r1.min, r2.min) to math.min(r1.max, r2.max)
}

object Action {
  def parse(s: String, max: Int = Int.MaxValue): Action = new ActionParser(s, max).inputline.run().get
}

class ActionParser(val input:ParserInput,max:Int) extends Parser {
  def clipRange(l:Int,u:Int):Range = math.max(l,-max) to math.min(u,max)
  def number:Rule1[Int] = rule {capture(optional("-") ~ oneOrMore(CharPredicate.Digit)) ~> ((s:String) => s.toInt)}
  def range:Rule1[Range] = rule {number ~ ".."~number ~> clipRange _}
  def cmd:Rule1[Int] = rule {("on" ~ push(1)) | ("off" ~ push(0)) }
  def action:Rule1[Action] = rule {cmd ~ " x=" ~ range ~ ",y="~range~",z="~range ~> Action.apply _}
  def inputline:Rule1[Action] = rule{action~EOI}
}
