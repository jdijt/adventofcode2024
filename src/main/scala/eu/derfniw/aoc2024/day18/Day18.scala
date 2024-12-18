package eu.derfniw.aoc2024.day18

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.mutable

case class Point(x: Int, y: Int):
  def neighbours: List[Point] =
    List(Point(x, y - 1), Point(x, y + 1), Point(x - 1, y), Point(x + 1, y))

/* Note: grid size is _inclusive_
 */
class Grid(val bytes: IndexedSeq[Point], val gridSize: Int):
  import Grid.given
  private val start = Point(0, 0)
  private val end   = Point(gridSize, gridSize)

  private inline def isOnGrid(p: Point): Boolean =
    p.x >= 0 && p.x <= gridSize && p.y >= 0 && p.y <= gridSize

  def shortestPath(bytesToConsider: Int): Option[Int] =
    val corruptedBytes = bytes.take(bytesToConsider).toSet
    val toVisit        = mutable.PriorityQueue((start, 0))
    val visisted       = mutable.Set.empty[Point]
    val distances      = mutable.Map(start -> 0)

    while toVisit.nonEmpty && !distances.contains(end) do
      val (point, distance) = toVisit.dequeue()
      if !visisted.contains(point) then
        visisted.add(point)
        point.neighbours.filter(p => isOnGrid(p) && !corruptedBytes.contains(p)).foreach {
          neighbour =>
            val newDistance = distance + 1
            if !distances.contains(neighbour) || newDistance < distances(neighbour) then
              distances(neighbour) = newDistance
              toVisit.enqueue(neighbour -> newDistance)
        }
    end while
    distances.get(end)
  end shortestPath

end Grid

object Grid:
  // REVERSED ordering for point + distance tuples
  private given Ordering[(Point, Int)] with
    def compare(x: (Point, Int), y: (Point, Int)): Int = y._2.compareTo(x._2)

  def fromInput(input: Seq[String], gridSize: Int): Grid =
    val bytes = input.collect { case s"$x,$y" => Point(x.toInt, y.toInt) }.toIndexedSeq
    Grid(bytes, gridSize)

def part1(gridSize: Int, readBytes: Int)(input: Seq[String]): Int =
  Grid.fromInput(input, gridSize).shortestPath(readBytes).get

// This can probably be made faster by:
// - Having the "shortestPath" return the set of points in the path
// - Only recomputing if we break the current path.
// But it now runs in 2-ish seconds for the real input, so its ok.
def part2(gridSize: Int, readBytes: Int)(input: Seq[String]): String =
  val grid = Grid.fromInput(input, gridSize)
  (readBytes to grid.bytes.size).view
    .map(l => (l, grid.shortestPath(l)))
    .collectFirst { case (l, None) =>
      val Point(x, y) = grid.bytes(l - 1)
      s"$x,$y"
    }
    .get
end part2

object Inputs extends InputReader(18)

@main
def day18(): Unit =
  println(s"Part 1: \n${runBenchmarked(Inputs.mainInput, part1(70, 1024)).pretty}")
  println(s"Part 2: \n${runBenchmarked(Inputs.mainInput, part2(70, 1024), 0).pretty}")
