package eu.derfniw.aoc2024.day08

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

case class Point(x: Int, y: Int)

class Grid(input: Seq[String]):
  private val ySize: Int = input.size
  private val xSize: Int = input.headOption.map(_.length).getOrElse(0)

  private val antannae: Map[Char, List[Point]] = input.zipWithIndex
    .flatMap { (line, y) =>
      line.zipWithIndex.collect {
        case (c, x) if c != '.' => c -> Point(x, y)
      }
    }
    .groupBy(_._1)
    .view
    .mapValues(_.map(_._2).toList)
    .toMap

  private def isOnGrid(p: Point): Boolean =
    p.x >= 0 && p.x < xSize && p.y >= 0 && p.y < ySize

  def countAntiNodes(): Int =
    val allAntiNodes = for
      antennaGroup            <- antannae.values
      Seq(antenna1, antenna2) <- antennaGroup.combinations(2)
      xDiff = antenna1.x - antenna2.x
      yDiff = antenna1.y - antenna2.y
      antinode <- Seq(
                    Point(antenna1.x + xDiff, antenna1.y + yDiff),
                    Point(antenna2.x - xDiff, antenna2.y - yDiff)
                  ).filter(isOnGrid)
    yield antinode

    allAntiNodes.toSet.size
  end countAntiNodes

  def countAntiNodes2(): Int =
    def antiNodes(xDiff: Int, yDiff: Int, start: Point): LazyList[Point] =
      start #:: antiNodes(xDiff, yDiff, Point(start.x + xDiff, start.y + yDiff))

    val allAntiNodes = for
      antennaGroup            <- antannae.values
      Seq(antenna1, antenna2) <- antennaGroup.combinations(2)
      xDiff = antenna1.x - antenna2.x
      yDiff = antenna1.y - antenna2.y
      antinode <- antiNodes(xDiff, yDiff, antenna1).takeWhile(isOnGrid)
                    ++ antiNodes(-xDiff, -yDiff, antenna2).takeWhile(isOnGrid)
    yield antinode

    allAntiNodes.toSet.size
  end countAntiNodes2
end Grid

def part1(input: Seq[String]): Int =
  Grid(input).countAntiNodes()

def part2(input: Seq[String]): Int =
  Grid(input).countAntiNodes2()

object Input extends InputReader(8)

@main
def day08(): Unit =
  println(s"Part 1:\n ${runBenchmarked(Input.mainInput, part1).pretty}")
  println(s"Part 2:\n ${runBenchmarked(Input.mainInput, part2).pretty}")
