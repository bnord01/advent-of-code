import Utils.seqFromFileLines

import scala.collection.mutable
import scala.math.Ordering.Implicits._
import org.parboiled2._

object Day16 {
  def main(args: Array[String]): Unit = {
    val input = seqFromFileLines("day16.txt")

    def versionSum(t:Term):Long = t match {
      case Literal(v,_) => v
      case Operator(v,_,ag) => v + ag.map(versionSum).sum
    }

    for (line <- input) {
      val term = Term(line)
      // Part 1
      println(versionSum(term))
      // Part 2
      println(term.value)
    }
  }
}

trait Term {
  def value:BigInt
}

object Term {

  implicit class WithToIntBin(s: String) {
    def toIntHex: Int = Integer.parseInt(s, 16)

    def toIntBin: Int = Integer.parseInt(s, 2)
    def toBigIntBin: BigInt = BigInt(s,2)
  }

  def charToBitString(c: Char): String = f"${c.toString.toIntHex.toBinaryString.toInt}%04d"

  def parseHex(s: String): String = s.flatMap(charToBitString)

  def apply(s: String): Term = new PacketParser(parseHex(s)).InputLine.run().get
}

case class Literal(version: Int, value: BigInt) extends Term

case class Operator(version: Int, op: Int, arguments: Seq[Term]) extends Term {
  override def value: BigInt = {
    val values = arguments.map(_.value)
    op match {
      case 0 => values.sum
      case 1 => values.product
      case 2 => values.min
      case 3 => values.max
      case 5 => if (values.head > values(1)) 1 else 0
      case 6 => if (values.head < values(1)) 1 else 0
      case 7 => if (values.head == values(1)) 1 else 0
    }
  }
}

class PacketParser(val input: ParserInput) extends Parser with StringBuilding {

  import Term.WithToIntBin

  def Bit = rule { "0" | "1" }

  def bits(i: Int) = rule { capture(i.times(Bit)) ~> { _.toIntBin } }

  def Version = rule { bits(3) }

  def Type = rule { bits(3) }

  def LitType = rule { Type ~> (i => test(i == 4)) }

  def LitBits = rule {
    push("") ~ // Put the empty string on the stack
      zeroOrMore("1" ~ capture(4.times(Bit)) ~> ((x: String, y) => x + y)) ~
      "0" ~ capture(4.times(Bit)) ~> ((x, y) => (x + y).toBigIntBin)
  }

  def Lit = rule { Version ~ LitType ~ LitBits ~> Literal }

  def OpType = rule { Type ~> (i => test(i != 4) ~ push(i)) }

  def OpLength = rule { "1" ~ bits(11) }

  def Op:Rule1[Operator] = rule {
    Version ~ OpType ~ ( // List of Terms
      "1" ~ bits(11) ~> (_.times(Trm) ~> (x => x)) | // Number of packages
        "0" ~ appendSB() ~ bits(15) ~> ( // Number of bits
          // The stack contains the number of bits left to parse and the list of terms parsed already
          i => push((i, Seq.empty[Term])) ~
              zeroOrMore(capture(Trm) ~>
                ((i: (Int, Seq[Term]), lit, st) => test(i._1 >= st.length) ~ push((i._1 - st.length, i._2 :+ lit))))
              ) ~> (x => x._2)
      ) ~> Operator
  }

  def Trm: Rule1[Term] = rule { Lit | Op }

  def InputLine = rule { Trm ~ zeroOrMore("0") ~ EOI }
}