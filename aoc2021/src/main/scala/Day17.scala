import scala.math.{abs, signum, sqrt}

case class Target(x: Seq[Int], y: Seq[Int])

case class Projectile(var x: Int, var y: Int, var dx: Int, var dy: Int) {
  def step: this.type = { x += dx; y += dy; dx -= signum(dx); dy -= 1; this }

  def in(target: Target): Boolean = target.x.contains(x) && target.y.contains(y)

  def beyond(target: Target): Boolean = {
    dx == 0 && !target.x.contains(x) ||
      dx > 0 && x > target.x.max ||
      dx < 0 && x < target.x.min ||
      dy < 0 && y < target.y.min
  }
}

object Projectile {
  def apply(dx: Int, dy: Int): Projectile = Projectile(0, 0, dx, dy)
}

object Day17 extends App {
  // val target = Target(20 to 30, -10 to -5) // example
  val target = Target(117 to 164, -140 to -89) // real

  // Part 1
  val dy0 = abs(target.y.min)-1
  println(dy0*(dy0+1)/2)

  // Part 2
  var count = 0

  for (dx <- sqrt(target.x.min).floor.toInt to target.x.max;
       dy <- target.y.min to abs(target.y.min)) {
    val proj = Projectile(dx, dy)
    var break = false
    while (!proj.step.beyond(target) && !break) {
      if (proj.in(target)) { count += 1; break = true }
    }
  }

  println(count)
}
