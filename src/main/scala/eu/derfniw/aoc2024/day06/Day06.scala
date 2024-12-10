package eu.derfniw.aoc2024.day06

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

enum Direction:
  case Up, Down, Left, Right

  def rotateLeft: Direction = this match
    case Direction.Up    => Direction.Right
    case Direction.Right => Direction.Down
    case Direction.Down  => Direction.Left
    case Direction.Left  => Direction.Up

class Grid(grid: IndexedSeq[IndexedSeq[Boolean]]):
  private val ySize = grid.size
  private val xSize = grid.headOption.map(_.size).getOrElse(0)

  def isObstacle(p: Point): Boolean = isOnGrid(p) && grid(p.y)(p.x)

  def isOnGrid(p: Point): Boolean = p.x >= 0 && p.x < xSize && p.y >= 0 && p.y < ySize

  def withObstacle(p: Point): Grid = Grid(grid.updated(p.y, grid(p.y).updated(p.x, true)))

  def allPoints: List[Point] =
    (0 until ySize).flatMap { y =>
      (0 until xSize).map { x =>
        Point(x, y)
      }
    }.toList
end Grid

case class Point(x: Int, y: Int):
  def next(direction: Direction): Point = direction match
    case Direction.Up    => Point(x, y - 1)
    case Direction.Down  => Point(x, y + 1)
    case Direction.Left  => Point(x - 1, y)
    case Direction.Right => Point(x + 1, y)

case class Guard(loc: Point, direction: Direction, locationsVisited: Set[Point], isOngrid: Boolean):
  def step(grid: Grid): Guard =
    val newLoc = loc.next(direction)
    if grid.isObstacle(newLoc) then copy(direction = direction.rotateLeft)
    else if !grid.isOnGrid(newLoc) then copy(loc = newLoc, isOngrid = false)
    else
      copy(
        loc = newLoc,
        locationsVisited = locationsVisited + newLoc,
        isOngrid = grid.isOnGrid(newLoc)
      )
  end step

  @tailrec
  final def walkRoute(grid: Grid): Guard =
    if isOngrid then step(grid).walkRoute(grid)
    else this
end Guard

case class Part2Guard(
    loc: Point,
    direction: Direction,
    locationsVisited: Set[(Point, Direction)],
    isOngrid: Boolean,
    isLooped: Boolean = false
):
  def step(grid: Grid): Part2Guard =
    val newLoc = loc.next(direction)
    if grid.isObstacle(newLoc) then copy(direction = direction.rotateLeft)
    else if !grid.isOnGrid(newLoc) then copy(loc = newLoc, isOngrid = false)
    else
      copy(
        loc = newLoc,
        locationsVisited = locationsVisited + (newLoc -> direction),
        isOngrid = grid.isOnGrid(newLoc),
        isLooped = locationsVisited((newLoc, direction))
      )
  end step

  @tailrec
  final def routeLoops(grid: Grid): Boolean =
    if !isOngrid then false
    else if isOngrid && isLooped then true
    else step(grid).routeLoops(grid)
end Part2Guard

def parseInput(input: Seq[String]): (Grid, Point) =
  val grid   = input.map(_.map(_ == '#'))
  val guardY = input.indexWhere(_.contains('^'))
  val guardX = input(guardY).indexOf('^')
  (Grid(grid.toIndexedSeq), Point(guardX, guardY))

def part1(input: Seq[String]): Int =
  val (grid, guardLocation) = parseInput(input)
  val guard                 = Guard(guardLocation, Direction.Up, Set(guardLocation), true)
  guard.walkRoute(grid).locationsVisited.size

def part2(input: Seq[String]): Int =
  val (grid, guardLocation) = parseInput(input)
  val part2Guard = Part2Guard(guardLocation, Direction.Up, Set((guardLocation, Direction.Up)), true)
  grid.allPoints.par.count { loc =>
    if grid.isObstacle(loc) then false
    else
      val newGrid = grid.withObstacle(loc)
      part2Guard.routeLoops(newGrid)
  }
end part2

object Inputs extends InputReader(6)

@main
def day06(): Unit =
  println(s"Part 1: ${runBenchmarked(Inputs.mainInput, part1).pretty}")
  println(s"Part 2: ${runBenchmarked(Inputs.mainInput, part2).pretty}")
