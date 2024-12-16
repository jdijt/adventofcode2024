package eu.derfniw.aoc2024.day16

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.parallel.CollectionConverters.*
import scala.util.Try

enum Direction:
  case Up, Down, Left, Right

  def rotateRight: Direction = this match
    case Direction.Up    => Direction.Right
    case Direction.Right => Direction.Down
    case Direction.Down  => Direction.Left
    case Direction.Left  => Direction.Up
  def rotateLeft: Direction = this match
    case Direction.Up    => Direction.Left
    case Direction.Left  => Direction.Down
    case Direction.Down  => Direction.Right
    case Direction.Right => Direction.Up
end Direction

enum MazeElement:
  case Wall, Empty, Start, End

object MazeElement:
  def fromChar(c: Char): MazeElement = c match
    case '#' => MazeElement.Wall
    case '.' => MazeElement.Empty
    case 'S' => MazeElement.Start
    case 'E' => MazeElement.End
    case _   => throw IllegalArgumentException(s"Unknown maze element: $c")

case class Point(x: Int, y: Int):
  def move(direction: Direction): Point = direction match
    case Direction.Up    => Point(x, y - 1)
    case Direction.Down  => Point(x, y + 1)
    case Direction.Left  => Point(x - 1, y)
    case Direction.Right => Point(x + 1, y)

class Maze(val map: IndexedSeq[IndexedSeq[MazeElement]]):
  val start: Point = map.zipWithIndex
    .flatMap { (row, y) =>
      row.zipWithIndex.collectFirst { case (MazeElement.Start, x) =>
        Point(x, y)
      }
    }
    .headOption
    .getOrElse(throw IllegalArgumentException("No start point found"))

  val end: Point = map.zipWithIndex
    .flatMap { (row, y) =>
      row.zipWithIndex.collectFirst { case (MazeElement.End, x) =>
        Point(x, y)
      }
    }
    .headOption
    .getOrElse(throw IllegalArgumentException("No end point found"))

  def valueAt(p: Point): MazeElement = map(p.y)(p.x)

  def cheapestRoute: Int =
    val memory = collection.mutable.Map.empty[Point, Option[Int]]
    def helper(position: Point, direction: Direction, visited: Set[Point]): Option[Int] =
      if visited.contains(position) then None
      else if memory.contains(position) then memory(position)
      else
        valueAt(position) match
          case MazeElement.End  => Some(0)
          case MazeElement.Wall => None
          case MazeElement.Empty | MazeElement.Start =>
            val newVisited = visited + position
            val res = Try(
              List(
                (direction, 1),
                (direction.rotateLeft, 1001),
                (direction.rotateRight, 1001)
              )
                .flatMap((dir, cost) => helper(position.move(dir), dir, newVisited).map(_ + cost))
                .min
            ).toOption
            memory.update(position, res)
            res

    helper(start, Direction.Right, Set.empty).get
  end cheapestRoute
end Maze

object Maze:
  def fromInput(input: Seq[String]): Maze =
    val map = input.map(_.map(MazeElement.fromChar).toIndexedSeq).toIndexedSeq
    Maze(map)

def part1(input: Seq[String]): Int =
  Maze.fromInput(input).cheapestRoute

def part2(input: Seq[String]): Int = ???

object Input extends InputReader(16)

@main
def day16(): Unit =
  println(s"Part1: \n${runBenchmarked(Input.mainInput, part1).pretty}")
