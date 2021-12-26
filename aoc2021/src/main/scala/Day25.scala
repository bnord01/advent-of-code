object Day25 extends App {
  type Field = Array[Array[Char]]
  val field: Field = Utils.seqFromFileLines("day25.txt").map(_.toCharArray).toArray

  def hstep() = {
    var moved = false
    for (i <- field.indices) {
      val v0 = field(i)(0)
      var just_moved = false
      val jmax = field(i).length - 1
      for (j <- 0 until jmax)
        if (field(i)(j) == '>' && field(i)(j + 1) == '.' && !just_moved) {
          field(i)(j + 1) = '>'
          field(i)(j) = '.'
          just_moved = true
          moved = true
        } else
          just_moved = false
      if (v0 == '.' && field(i)(jmax) == '>' && !just_moved) {
        field(i)(jmax) = '.'
        field(i)(0) = '>'
        moved = true
      }
    }
    moved
  }

  def vstep() = {
    var moved = false
    for (j <- field(0).indices) {
      val v0 = field(0)(j)
      var just_moved = false
      val imax = field.length - 1
      for (i <- 0 until imax)
        if (field(i)(j) == 'v' && field(i + 1)(j) == '.' && !just_moved) {
          field(i + 1)(j) = 'v'
          field(i)(j) = '.'
          just_moved = true
          moved = true
        } else
          just_moved = false
      if (v0 == '.' && field(imax)(j) == 'v' && !just_moved) {
        field(imax)(j) = '.'
        field(0)(j) = 'v'
        moved = true
      }
    }
    moved
  }

  def step() = {
    hstep() | vstep()
  }

  var cnt = 1
  while (step()) cnt += 1
  println(cnt)
}
