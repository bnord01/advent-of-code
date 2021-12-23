import State.{dest, is_floor, is_room, positions}

import scala.collection.immutable.LazyList
import scala.collection.mutable

object Day23 extends App {

  def dijkstra(start: State): Int = {
    implicit object StatePriority extends Ordering[(Int, State)] {
      override def compare(x: (Int, State), y: (Int, State)): Int = -Integer.compare(x._1, y._1)
    }
    val wl = mutable.PriorityQueue((0, start))
    val rdy = mutable.Set[State]()
    while (wl.nonEmpty) {
      val (c, s) = wl.dequeue()
      if (!rdy(s)) {
        rdy += s
        if (s.done)
          return c
        else
          wl ++= s.moves.map { case (c1, s1) => (c + c1, s1) }
      }
    }
    -1
  }

  /* val example_input1 = State(Map(
    2 -> (1 :: 0 :: Nil),
    4 -> (2 :: 3 :: Nil),
    6 -> (1 :: 2 :: Nil),
    8 -> (3 :: 0 :: Nil)), Map(), 2)
  println(dijkstra(example_input1)) */

  val input1 = State(Map(
      2 -> (0 :: 2 :: Nil),
      4 -> (3 :: 2 :: Nil),
      6 -> (0 :: 3 :: Nil),
      8 -> (1 :: 1 :: Nil)), Map(), 2)
  println(dijkstra(input1))

  /* val example_input2 = State(Map(
      2 -> (1 :: 3 :: 3 :: 0 :: Nil),
      4 -> (2 :: 2 :: 1 :: 3 :: Nil),
      6 -> (1 :: 1 :: 0 :: 2 :: Nil),
      8 -> (3 :: 0 :: 2 :: 0 :: Nil)), Map(), 4)
  println(dijkstra(example_input2)) */

  val input2 = State(Map(
      2 -> (0 :: 3 :: 3 :: 2 :: Nil),
      4 -> (3 :: 2 :: 1 :: 2 :: Nil),
      6 -> (0 :: 1 :: 0 :: 3 :: Nil),
      8 -> (1 :: 0 :: 2 :: 1 :: Nil)), Map(), 4)
  println(dijkstra(input2))
}

case class State(room:Map[Int,List[Int]],floor:Map[Int,Int], room_size:Int) {
  def fill_room(i:Int) :Boolean = room(i).forall(v => dest(v) == i)
  def can_move_to(i:Int): Boolean = if (is_floor(i)) !floor.keySet(i) else fill_room(i)
  def can_move_through(i:Int): Boolean = is_room(i) || !floor.keySet(i)
  def path(i:Int,j:Int): Range = if (i < j) i+1 until j else j+1 until i
  def can_move(i:Int, j:Int): Boolean = i != j && can_move_to(j) && path(i,j).forall(can_move_through)

  def room_done(i:Int): Boolean = {val ri = room(i); ri.sizeIs == room_size && ri.forall(v => dest(v) == i)}

  def move_out(i:Int,j:Int): Option[(Int,State)] =  {
    val ri = room(i)
    if (is_floor(j) && ri.nonEmpty && (!room_done(i)) && can_move(i,j)) {
      val cost = math.pow(10,ri.head).toInt*(math.abs(i-j) + (room_size+1 - ri.length))
      val state = State(room + (i -> ri.tail), floor + (j -> ri.head),room_size)
     Some((cost,state))
    } else None
  }

  def move_in(i:Int): Option[(Int,State)] = {
    val v = floor(i); val j = dest(v)
    if (can_move(i,j)) {
      val cost = math.pow(10, v).toInt * (math.abs(i - j) + (room_size - room(j).length))
      val state = State(room + (j -> (v :: room(j))), floor - i, room_size)
      Some((cost, state))
    } else None
  }

  def out_moves: Seq[(Int, State)] = for ((i,r) <- LazyList.from(room) if r.nonEmpty; j <- positions; r <- move_out(i,j)) yield r

  def in_moves: Seq[(Int, State)] = for (i <- LazyList.from(floor.keys); r <- move_in(i)) yield r

  def moves: Seq[(Int, State)] = in_moves ++ out_moves

  def done: Boolean = floor.isEmpty && room.forall{ case (i,l) => l.forall(v => dest(v) == i) }
}

object State {
  val first_room = 2
  val num_rooms = 4
  val floor_width: Int =  2 * num_rooms + 3
  val is_room: Array[Boolean] = Array.tabulate(floor_width)(i => i >= first_room && ((i -first_room) % 2 == 0) && i < first_room + num_rooms*2)
  val is_floor: Array[Boolean] = Array.tabulate(floor_width)(i => !is_room(i))
  val dest: Array[Int] = Array.tabulate(num_rooms)(i => first_room+2*i)
  val positions: Range = 0 until floor_width
}