package eu.derfniw.aoc2024.day15

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

case class Loc(x: Int, y: Int):
  def move(move: Move): Loc = move match
    case Move.Up    => Loc(x, y - 1)
    case Move.Down  => Loc(x, y + 1)
    case Move.Left  => Loc(x - 1, y)
    case Move.Right => Loc(x + 1, y)

enum Obj:
  case Empty, Box, Wall, Robot, BoxLeft, BoxRight

  override def toString: String = this match
    case Empty    => "."
    case Box      => "O"
    case Wall     => "#"
    case Robot    => "@"
    case BoxLeft  => "["
    case BoxRight => "]"
end Obj

object Obj:
  def fromChar(c: Char): Obj = c match
    case '.' => Empty
    case 'O' => Box
    case '#' => Wall
    case '@' => Robot
    case _   => throw IllegalArgumentException(s"Unknown object: $c")

enum Move:
  case Up, Down, Left, Right

object Move:
  def fromChar(c: Char): Move = c match
    case '^' => Up
    case 'v' => Down
    case '<' => Left
    case '>' => Right
    case _   => throw IllegalArgumentException(s"Unknown move: $c")

class Grid(val grid: IndexedSeq[IndexedSeq[Obj]], robot: Loc):

  private def valueAt(loc: Loc): Obj = grid(loc.y)(loc.x)

  def applyMove(move: Move): Grid = applyMove(robot, move)

  private def applyMove(loc: Loc, move: Move): Grid =
    val target = loc.move(move)
    val value  = valueAt(loc)

    def moveToTarget(g: IndexedSeq[IndexedSeq[Obj]]) =
      val cleared = g.updated(loc.y, g(loc.y).updated(loc.x, Obj.Empty))
      cleared.updated(target.y, cleared(target.y).updated(target.x, value))

    valueAt(target) match
      case Obj.Empty =>
        Grid(moveToTarget(grid), if value == Obj.Robot then target else loc)
      case Obj.Box =>
        val ng = applyMove(target, move)
        ng.valueAt(target) match
          case Obj.Empty =>
            Grid(moveToTarget(ng.grid), if value == Obj.Robot then target else loc)
          case _ => this
      case Obj.BoxLeft | Obj.BoxRight if move == Move.Left || move == Move.Right =>
        val ng = applyMove(target, move)
        ng.valueAt(target) match
          case Obj.Empty =>
            Grid(moveToTarget(ng.grid), if value == Obj.Robot then target else loc)
          case _ => this
      // Move will be up or down here)
      case Obj.BoxLeft =>
        val extraTarget = target.move(Move.Right)
        val ng          = applyMove(target, move).applyMove(extraTarget, move)
        (ng.valueAt(target), ng.valueAt(extraTarget)) match
          case (Obj.Empty, Obj.Empty) =>
            Grid(moveToTarget(ng.grid), if value == Obj.Robot then target else loc)
          case _ => this
      case Obj.BoxRight =>
        val extraTarget = target.move(Move.Left)
        val ng          = applyMove(target, move).applyMove(extraTarget, move)
        (ng.valueAt(target), ng.valueAt(extraTarget)) match
          case (Obj.Empty, Obj.Empty) =>
            Grid(moveToTarget(ng.grid), if value == Obj.Robot then target else loc)
          case _ => this
      case _ => this
    end match
  end applyMove

  def sumCoordinates: Int =
    grid.zipWithIndex.flatMap { (row, y) =>
      row.zipWithIndex.collect { case (Obj.Box | Obj.BoxLeft, x) => x + (100 * y) }
    }.sum

  def scaleUp: Grid =
    val newGrid = grid.map {
      _.flatMap {
        case Obj.Box   => Seq(Obj.BoxLeft, Obj.BoxRight)
        case Obj.Wall  => Seq(Obj.Wall, Obj.Wall)
        case Obj.Robot => Seq(Obj.Robot, Obj.Empty)
        case Obj.Empty => Seq(Obj.Empty, Obj.Empty)
        case _ =>
          throw IllegalArgumentException(
            "Found object belonging to expanded grid, cannot scale up grid twice!"
          )
      }
    }
    val (rX, rY) = newGrid.zipWithIndex.flatMap { (row, y) =>
      row.zipWithIndex.collectFirst { case (Obj.Robot, x) => (x, y) }
    }.head
    Grid(newGrid, Loc(rX, rY))
  end scaleUp

end Grid

object Grid:
  def fromInput(input: Seq[String]): Grid =
    val grid = input.map(_.map(Obj.fromChar).toIndexedSeq).toIndexedSeq
    val (rX, rY) = grid.zipWithIndex.flatMap { (row, y) =>
      row.zipWithIndex.collectFirst { case (Obj.Robot, x) => (x, y) }
    }.head
    Grid(grid, Loc(rX, rY))

end Grid

def parseInput(input: Seq[String]): (Grid, Seq[Move]) =
  val splitIndex = input.indexWhere(_.isEmpty)
  val grid       = Grid.fromInput(input.slice(0, splitIndex))
  val moves      = input.slice(splitIndex + 1, input.length).flatMap(_.map(Move.fromChar))
  (grid, moves)

def part1(input: Seq[String]): Int =
  val (grid, moves) = parseInput(input)
  moves
    .foldLeft(grid)((g, m) => g.applyMove(m))
    .sumCoordinates
end part1

def part2(input: Seq[String]): Int =
  val (grid, moves) = parseInput(input)
  val expandedGrid  = grid.scaleUp
  moves
    .foldLeft(expandedGrid)((g, m) => g.applyMove(m))
    .sumCoordinates
end part2

object Input extends InputReader(15)

@main
def day15(): Unit =
  println(s"Part 1: \n${runBenchmarked(Input.mainInput, part1).pretty}")
  println(s"Part 2: \n${runBenchmarked(Input.mainInput, part2).pretty}")
