package eu.derfniw.aoc2024.day12

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.mutable

case class Point(x: Int, y: Int)

class Grid(map: IndexedSeq[IndexedSeq[Char]]):
  private val ySize = map.size
  private val xSize = map.headOption.map(_.size).getOrElse(0)

  private inline def isOnGrid(point: Point): Boolean   = isOnGrid(point.x, point.y)
  private inline def isOnGrid(x: Int, y: Int): Boolean = x >= 0 && x < xSize && y >= 0 && y < ySize

  private inline def valueAt(point: Point): Char   = valueAt(point.x, point.y)
  private inline def valueAt(x: Int, y: Int): Char = if isOnGrid(x, y) then map(y)(x) else '.'

  inline def getNeighbours(point: Point): List[Point] =
    List(
      Point(point.x, point.y - 1),
      Point(point.x, point.y + 1),
      Point(point.x - 1, point.y),
      Point(point.x + 1, point.y)
    )

  inline def getNeighbourValues(point: Point): List[(Point, Char)] =
    getNeighbours(point).map(p => p -> valueAt(p))

  def regions: Seq[(Char, Seq[Point])] =
    val visited = mutable.Set.empty[Point]
    val regions = mutable.ArrayDeque.empty[(Char, Seq[Point])]

    for y <- 0 until ySize; x <- 0 until xSize do
      val point = Point(x, y)
      if !visited.contains(point) then
        val regionValue = valueAt(point)
        val region      = mutable.Set(point)
        val toVisit     = mutable.ArrayDeque(point)
        while toVisit.nonEmpty do
          val current = toVisit.removeHead()
          getNeighbourValues(current)
            .filter { case (_, v) => v == regionValue }
            .foreach { case (neighbour, _) =>
              if !region.contains(neighbour) then
                region += neighbour
                toVisit += neighbour
            }
        end while
        visited ++= region
        regions += regionValue -> region.toSeq
      end if
    end for

    regions.toSeq
  end regions

end Grid

object Grid:
  def fromInput(input: Seq[String]): Grid =
    Grid(input.map(_.toIndexedSeq).toIndexedSeq)

def part1(input: Seq[String]): Int =
  val grid    = Grid.fromInput(input)
  val regions = grid.regions
  regions.map { case (value, region) =>
    val size = region.size
    val perimeter = region.view
      .map(p => grid.getNeighbourValues(p).count { case (_, v) => v != value })
      .sum
    size * perimeter
  }.sum
end part1

def part2(input: Seq[String]): Int = ???

object Inputs extends InputReader(12)

@main
def day11(): Unit =
  println(s"Part1:\n ${runBenchmarked(Inputs.mainInput, part1).pretty}")
  // println(s"Part2:\n ${runBenchmarked(Inputs.mainInput, part2).pretty}")
