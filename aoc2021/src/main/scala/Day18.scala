import Utils.seqFromFileLines

import scala.collection.mutable
import scala.math.Ordering.Implicits._
import org.parboiled2._

import scala.math.max

import scala.math.ceil

object Day18 {
  def main(args: Array[String]): Unit = {
    val input = seqFromFileLines("day18.txt")
    val numbers = input.map(SFNumber.apply)

    // Part 1
    println(numbers.reduce { _ + _ }.magnitude)

    // Part 2
    println((numbers zip numbers.tails.drop(1))
      .flatMap{ case (n, tail) => tail.flatMap{ m => Seq((n + m).magnitude, (m + n).magnitude) } }.max)
  }
}

trait SFNumber {
  def explode(depth: Int = 0): (Option[Int], SFNumber, Option[Int])
  def addLeft(v: Int): SFNumber
  def addRight(v: Int): SFNumber
  def split: Option[SFNumber]
  def magnitude: Long
  def depth: Int

  def +(other: SFNumber): SFNumber = {
    SFPair(this, other).reduce
  }

  def reduce: SFNumber = {
    var change = false
    var curr = this
    do {
      change = false
      if (curr.depth > 4) {
        curr = curr.explode()._2
        change = true
      } else curr.split foreach {
        ncurr => {
          curr = ncurr
          change = true
        }
      }
    } while (change)
    curr
  }
}

object SFNumber {
  def apply(s: String): SFNumber = new SFNumberParser(s).InputLine.run().get
}

case class SFLiteral(value: Int) extends SFNumber {
  override def depth = 0
  override def toString: String = value.toString
  override def explode(depth: Int): Nothing = throw new UnsupportedOperationException()
  override def split: Option[SFNumber] = if (value < 10) None else Some(SFPair(SFLiteral(value / 2), SFLiteral(ceil(value / 2d).toInt)))
  override def addLeft(v: Int): SFNumber = SFLiteral(value + v)
  override def addRight(v: Int): SFNumber = SFLiteral(value + v)
  override def magnitude: Long = value
}

case class SFPair(left: SFNumber, right: SFNumber) extends SFNumber {
  override def toString: String = f"[$left,$right]"
  override def depth: Int = max(left.depth, right.depth) + 1

  override def explode(depth: Int): (Option[Int], SFNumber, Option[Int]) = {
    if (depth == 4) {
      val vl = left match { case SFLiteral(v) => v }
      val vr = right match { case SFLiteral(v) => v }
      (Some(vl), SFLiteral(0), Some(vr))
    } else if (depth + left.depth == 4) {
      val (ol, nleft, or) = left.explode(depth + 1)
      val nright = or.map(right.addLeft).getOrElse(right)
      (ol, SFPair(nleft, nright), None)
    } else if (depth + right.depth == 4) {
      val (ol, nright, or) = right.explode(depth + 1)
      val nleft = ol.map(left.addRight).getOrElse(left)
      (None, SFPair(nleft, nright), or)
    } else throw new UnsupportedOperationException()
  }

  override def addLeft(v: Int): SFNumber = SFPair(left.addLeft(v), right)
  override def addRight(v: Int): SFNumber = SFPair(left, right.addRight(v))
  override def split: Option[SFNumber] =
    left.split.map(SFPair(_, right)).orElse(right.split.map(SFPair(left, _)))
  override def magnitude: Long = 3 * left.magnitude + 2 * right.magnitude
}

class SFNumberParser(val input: ParserInput) extends Parser {
  def Literal: Rule1[SFLiteral] = rule { capture(Digits) ~> ((d: String) => SFLiteral(d.toInt)) }
  def Pair: Rule1[SFPair] = rule { "[" ~ Number ~ "," ~ Number ~ "]" ~> SFPair }
  def Number: Rule1[SFNumber] = rule { Literal | Pair }
  def Digits: Rule0 = rule { oneOrMore(CharPredicate.Digit) }
  def InputLine: Rule1[SFNumber] = rule { Number ~ EOI }
}