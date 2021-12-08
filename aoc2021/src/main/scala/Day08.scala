import Utils.seqFromFileLines

object Day08 extends App {
  val input = seqFromFileLines("day08.txt")
  val codes = input.map {
    _.split('|').map {
      _.trim.split(' ').map { c => Set.from(c.trim) }
    } match {
      case Array(u, c) => (Set.from(u.sortWith((a, b) => a.size < b.size)), c)
    }
  }

  // Part 1
  val count1 = codes.map { case (_, c) => c.count { _.sizeIs == 2 } }.sum
  val count4 = codes.map { case (_, c) => c.count { _.sizeIs == 4 } }.sum
  val count7 = codes.map { case (_, c) => c.count { _.sizeIs == 3 } }.sum
  val count8 = codes.map { case (_, c) => c.count { _.sizeIs == 7 } }.sum

  println(count1 + count4 + count7 + count8)

  // Part 2
  codes.map {
    case (u, c) => {
      val one = u.find(_.sizeIs == 2).get
      val four = u.find(_.sizeIs == 4).get
      val seven = u.find(_.sizeIs == 3).get
      val eight = u.find(_.sizeIs == 7).get

      var openSize6 = u.filter {
        _.sizeIs == 6
      } // 0 6 9
      var openSize5 = u.filter {
        _.sizeIs == 5
      } // 2 3 5

      val six = openSize6.find { s => eight -- s subsetOf one }.get
      openSize6 -= six // 0 9

      val five = openSize5.find { s => s subsetOf six }.get
      openSize5 -= five // 2 3

      val zero = openSize6.find { s => (s -- five).sizeIs == 2 }.get
      openSize6 -= zero // 9

      val nine = openSize6.head

      val three = openSize5.find { s => s subsetOf nine }.get
      openSize5 -= three

      val two = openSize5.head

      val decode = Map(zero -> 0, one -> 1, two -> 2, three -> 3, four -> 4, five -> 5, six -> 6, seven -> 7, eight -> 8, nine -> 9)

      val result = c.map(decode).foldRight((0, 1)) { case (c, (acc, b)) => (acc + c * b, b * 10) }._1

      result
    }
  }.sum match {
    case r => println(r)
  }

}