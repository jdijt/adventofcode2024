package eu.derfniw.aoc2024.day20

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.mutable

case class Point(x: Int, y: Int):
  def neighbours: List[Point] =
      List(Point(x, y - 1), Point(x, y + 1), Point(x - 1, y), Point(x + 1, y))

  def distance(p: Point): Int = (x - p.x).abs + (y - p.y).abs
end Point

enum Tile:
  case Empty, Wall, Start, End

object Tile:
  def fromChar(c: Char): Tile = c match
    case '.' => Empty
    case '#' => Wall
    case 'S' => Start
    case 'E' => End

class Maze(map: IndexedSeq[IndexedSeq[Tile]]):
  private val ySize = map.size
  private val xSize = map.headOption.map(_.size).getOrElse(0)
  private val start = map.zipWithIndex.flatMap { (row, y) =>
    row.zipWithIndex.collectFirst { case (Tile.Start, x) => Point(x, y) }
  }.head
  private val end = map.zipWithIndex.flatMap { (row, y) =>
    row.zipWithIndex.collectFirst { case (Tile.End, x) => Point(x, y) }
  }.head

  private inline def isOnMap(p: Point): Boolean =
    p.x >= 0 && p.x < xSize && p.y >= 0 && p.y < ySize

  private inline def valueAt(p: Point): Tile = if isOnMap(p) then map(p.y)(p.x) else Tile.Wall

  lazy val path: Map[Point, Int] =
    val toVisit = mutable.ArrayDeque(start)
    val visited = mutable.Set.empty[Point]
    val path    = mutable.ArrayBuffer.empty[Point]

    while toVisit.nonEmpty && !path.lastOption.contains(end) do
      val current = toVisit.removeHead()
      visited.add(current)
      path.addOne(current)
      current.neighbours
        .filter(p => valueAt(p) != Tile.Wall && !visited.contains(p))
        .foreach(toVisit.addOne)
    end while
    // There is a single path, idx == length.
    path.zipWithIndex.toMap
  end path

  private def skipsCandidatesFrom(source: Point, maxLenght: Int): Seq[Point] =
    (-maxLenght to maxLenght)
      .flatMap { yOffset =>
        val xBudget = maxLenght - yOffset.abs
        (-xBudget to xBudget).map(xOffset => Point(source.x + xOffset, source.y + yOffset))
      }
      .filter(p => path.contains(p) && p != source)

  def skips(maxLength: Int): Seq[Int] = path.keys.view.flatMap { start =>
    skipsCandidatesFrom(start, maxLength)
      .map(skip => (start, skip, path(skip) - (path(start) + start.distance(skip))))
      .collect { case (s, e, diff) if diff >= 0 && s.distance(e) <= diff => diff }
  }.toSeq
end Maze

object Maze:
  def fromInput(input: Seq[String]): Maze =
    val map = input.map(_.map(Tile.fromChar).toIndexedSeq).toIndexedSeq
    Maze(map)
end Maze

def part1(input: Seq[String]): Int =
  val maze  = Maze.fromInput(input)
  val skips = maze.skips(2)
  skips.count(_ >= 100)
end part1

def part2(input: Seq[String]): Int =
  val maze  = Maze.fromInput(input)
  val skips = maze.skips(20)
  skips.count(_ >= 100)

object Inputs extends InputReader(20)

@main
def day20(): Unit =
  println(s"Part 1: \n${runBenchmarked(Inputs.mainInput, part1).pretty}")
  println(s"Part 2: \n${runBenchmarked(Inputs.mainInput, part2).pretty}")
