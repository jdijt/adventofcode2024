package eu.derfniw.aoc2024.day10

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

case class Point(x: Int, y: Int)

class Map(val grid: IndexedSeq[IndexedSeq[Int]]):
  private val ySize = grid.size
  private val xSize = grid.headOption.map(_.size).getOrElse(0)

  def trailHeads(): Seq[Point] = grid.zipWithIndex.flatMap { case (row, y) =>
    row.zipWithIndex.collect { case (0, x) => Point(x, y) }
  }

  def heightAt(point: Point): Int = if isOnGrid(point) then grid(point.y)(point.x) else 0

  def isOnGrid(point: Point): Boolean =
    point.x >= 0 && point.x < xSize && point.y >= 0 && point.y < ySize

  def trailHeadScore(trailHead: Point): Int =
    val peaksFound = collection.mutable.Set.empty[Point]
    val visited    = collection.mutable.Set(trailHead)
    val toVisit    = collection.mutable.Queue(trailHead)

    while toVisit.nonEmpty do
      val current = toVisit.removeHead()
      if heightAt(current) == 9 then peaksFound += current
      else
        val nextPoints = Seq(
          Point(current.x, current.y - 1),
          Point(current.x, current.y + 1),
          Point(current.x - 1, current.y),
          Point(current.x + 1, current.y)
        )
        for next <- nextPoints do
          if isOnGrid(next) && heightAt(next) == heightAt(current) + 1 && !visited.contains(next)
          then
            toVisit += next
            visited += next
      end if
    end while
    peaksFound.size
  end trailHeadScore
end Map

object Map:
  def fromInput(input: Seq[String]): Map = Map(
    input.map(_.map(_.asDigit).toIndexedSeq).toIndexedSeq
  )

object Inputs extends InputReader(10)

def part1(input: Seq[String]): Int =
  val map        = Map.fromInput(input)
  val trailHeads = map.trailHeads()
  val scores     = trailHeads.map(map.trailHeadScore)
  scores.sum

@main
def day10(): Unit =
  println(s"Part 1:\n ${runBenchmarked(Inputs.mainInput, part1).pretty}")
