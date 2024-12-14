package eu.derfniw.aoc2024.day08

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

case class Point(x: Int, y: Int)

class Grid(input: Seq[String]):
  private val ySize: Int = input.size
  private val xSize: Int = input.headOption.map(_.length).getOrElse(0)

  private val antennaePairs: Set[(Point, Point)] = input.zipWithIndex
    .flatMap { (line, y) =>
      line.zipWithIndex.collect {
        case (c, x) if c != '.' => c -> Point(x, y)
      }
    }
    .groupBy(_._1)
    .values
    .flatMap(_.map(_._2).combinations(2).map { case a +: b +: _ => a -> b })
    .toSet

  def countAntiNodes(): Int =
    val allAntiNodes = for
      (antenna1, antenna2) <- antennaePairs
      xDiff = antenna1.x - antenna2.x
      yDiff = antenna1.y - antenna2.y
      antiNode <- Seq(
                    Point(antenna1.x + xDiff, antenna1.y + yDiff),
                    Point(antenna2.x - xDiff, antenna2.y - yDiff)
                  ).filter(isOnGrid)
    yield antiNode

    allAntiNodes.size
  end countAntiNodes

  private inline def isOnGrid(p: Point): Boolean =
    p.x >= 0 && p.x < xSize && p.y >= 0 && p.y < ySize

  def countAntiNodes2(): Int =
    def antiNodes(xDiff: Int, yDiff: Int, start: Point): List[Point] =
      if !isOnGrid(start) then Nil
      else start :: antiNodes(xDiff, yDiff, Point(start.x + xDiff, start.y + yDiff))

    val allAntiNodes = for
      (antenna1, antenna2) <- antennaePairs
      xDiff = antenna1.x - antenna2.x
      yDiff = antenna1.y - antenna2.y
      antiNode <- antiNodes(xDiff, yDiff, antenna1) ++ antiNodes(-xDiff, -yDiff, antenna2)
    yield antiNode

    allAntiNodes.size
  end countAntiNodes2
end Grid

def part1(input: Seq[String]): Int =
  Grid(input).countAntiNodes()

def part2(input: Seq[String]): Int =
  Grid(input).countAntiNodes2()

object Input extends InputReader(8)

@main
def day08(): Unit =
  println(s"Part 1: \n${runBenchmarked(Input.mainInput, part1).pretty}")
  println(s"Part 2: \n${runBenchmarked(Input.mainInput, part2).pretty}")
