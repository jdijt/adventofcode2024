package eu.derfniw.aoc2024.day16

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

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
  import Maze.{PointDir, given}

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

  def shortestPathToEnd: Int =
    val toProcess = mutable.PriorityQueue[(PointDir, Int)]((start, Direction.Right) -> 0)
    val visited   = mutable.Set.empty[PointDir]
    val distances = mutable.Map((start, Direction.Right) -> 0)

    while toProcess.nonEmpty do
      val (curPointDir, distance) = toProcess.dequeue()
      if !visited.contains(curPointDir) then
        visited.add(curPointDir)
        val (curPoint, curDir) = curPointDir
        List(
          ((curPoint.move(curDir), curDir), 1),
          ((curPoint.move(curDir.rotateLeft), curDir.rotateLeft), 1001),
          ((curPoint.move(curDir.rotateRight), curDir.rotateRight), 1001)
        ).filter { case (pd @ (p, _), _) =>
          valueAt(p) != MazeElement.Wall && !visited.contains(pd)
        }.foreach { case (pd, extraDistance) =>
          val newDist = distance + extraDistance
          if !distances.contains(pd) || newDist < distances(pd) then
            distances(pd) = newDist
            toProcess.enqueue((pd, newDist))
        }
      end if
    end while
    distances.collect { case ((p, _), d) if p == end => d }.min
  end shortestPathToEnd

  def positionsOnPath: Int =
    val toProcess = mutable.PriorityQueue[(PointDir, Int)]((start, Direction.Right) -> 0)
    val visited   = mutable.Set.empty[PointDir]
    val distances = mutable.Map((start, Direction.Right) -> 0)
    val previous  = mutable.Map.empty[PointDir, Set[PointDir]].withDefaultValue(Set.empty)

    while toProcess.nonEmpty do
      val (curPointDir, distance) = toProcess.dequeue()
      if !visited.contains(curPointDir) then
        visited.add(curPointDir)
        val (curPoint, curDir) = curPointDir
        List(
          ((curPoint.move(curDir), curDir), 1),
          ((curPoint.move(curDir.rotateLeft), curDir.rotateLeft), 1001),
          ((curPoint.move(curDir.rotateRight), curDir.rotateRight), 1001)
        ).filter { case (pd @ (p, _), _) =>
          valueAt(p) != MazeElement.Wall && !visited.contains(pd)
        }.foreach { case (pd, extraDistance) =>
          val newDist = distance + extraDistance
          if !distances.contains(pd) || newDist < distances(pd) then
            distances(pd) = newDist
            toProcess.enqueue((pd, newDist))
            previous(pd) = Set(curPointDir)
          else if newDist == distances(pd) then previous(pd) += curPointDir
          end if
        }
      end if
    end while
    val (minPd, dist) = distances
      .collect {
        case (pd @ (p, _), d) if p == end => (pd, d)
      }
      .minBy(_._2)

    val pointsToCheck = mutable.ArrayDeque(minPd)
    val visitedPoints = mutable.Set(minPd)
    while pointsToCheck.nonEmpty do
      val curPd = pointsToCheck.removeHead()
      previous(curPd).foreach { prevPd =>
        if !visitedPoints.contains(prevPd) then
          visitedPoints.add(prevPd)
          pointsToCheck.addOne(prevPd)
        end if
      }
    end while
    visitedPoints.map(_._1).size
  end positionsOnPath
end Maze

object Maze:
  def fromInput(input: Seq[String]): Maze =
    val map = input.map(_.map(MazeElement.fromChar).toIndexedSeq).toIndexedSeq
    Maze(map)

  private type PointDir = (Point, Direction)
  private given Ordering[(PointDir, Int)] with
    def compare(x: (PointDir, Int), y: (PointDir, Int)): Int =
      // Note: this is _REVERSED_
      y._2.compareTo(x._2)
end Maze

def part1(input: Seq[String]): Int =
  Maze.fromInput(input).shortestPathToEnd

def part2(input: Seq[String]): Int =
  Maze.fromInput(input).positionsOnPath

object Input extends InputReader(16)

@main
def day16(): Unit =
  println(s"Part1: \n${runBenchmarked(Input.mainInput, part1).pretty}")
  println(s"Part2: \n${runBenchmarked(Input.mainInput, part2).pretty}")
