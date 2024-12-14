package eu.derfniw.aoc2024.day14

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

case class Robot(x: Long, y: Long, dx: Long, dy: Long):
  def move(tics: Long, xBorder: Long, yBorder: Long): Robot =
    val newX = (x + (dx * tics)) % xBorder
    val newY = (y + (dy * tics)) % yBorder

    copy(
      x = if newX < 0 then xBorder + newX else newX,
      y = if newY < 0 then yBorder + newY else newY
    )
end Robot

object Robot:
  private val linePattern = """p=(\d+),(\d+) v=(-?\d+),(-?\d+)""".r
  def fromLine(line: String): Robot = line match
    case linePattern(x, y, dx, dy) => Robot(x.toLong, y.toLong, dx.toLong, dy.toLong)

case class Grid(maxX: Long, maxY: Long, bots: Seq[Robot]):

  def move(tics: Long): Grid =
    copy(bots = bots.map(_.move(tics, maxX, maxY)))

  def getQuadrantScore: Int =
    val xCenter        = maxX / 2
    val yCenter        = maxY / 2
    var q1, q2, q3, q4 = 0
    for bot <- bots do
      if bot.x < xCenter then
        if bot.y < yCenter then q1 += 1
        else if bot.y > yCenter then q3 += 1
      else if bot.x > xCenter then
        if bot.y < yCenter then q2 += 1
        else if bot.y > yCenter then q4 += 1
    q1 * q2 * q3 * q4
  end getQuadrantScore

  def noOverLap: Boolean =
    val botPositions = bots.map(bot => (bot.y, bot.x)).to(Set)
    botPositions.size == bots.size

  def mkString: String =
    val ps = bots.groupBy(_.y).map((y, bots) => (y, bots.map(_.x).toSet)).toMap
    (0 until maxY.toInt)
      .map(y =>
        (0 until maxX.toInt)
          .map(x => if ps.contains(y) && ps(y).contains(x) then '#' else '.')
          .mkString
      )
      .mkString("\n")
  end mkString
end Grid

object Grid:
  def fromInput(maxX: Int, maxY: Int, input: Seq[String]): Grid =
    val bots = input.map(Robot.fromLine)
    Grid(maxX, maxY, bots)

object Input extends InputReader(14)

def part1(maxX: Int, maxY: Int)(input: Seq[String]): Int =
  Grid.fromInput(maxX, maxY, input).move(100).getQuadrantScore

def part2(maxX: Int, maxY: Int)(input: Seq[String]): String =
  val grid = Grid.fromInput(maxX, maxY, input)
  // At maxX * maxY the bots will all have cycled back to their original position.
  // This holds because maxX and maxY are prime numbers (in all provided samples).
  val tics = 0 to maxX * maxY
  // Assumption: there are very few moments when all bots are in distinct locations.
  //      One of these moments is when all bots are in the christmas tree shape
  "\n" + tics
    .map(t => (t, grid.move(t)))
    .filter((_, g) => g.noOverLap)
    .map((t, g) => s"Found at $t: \n${g.mkString}")
    .mkString("\n\n")

end part2

@main
def day14(): Unit =
  println(s"Part 1:\n ${runBenchmarked(Input.mainInput, part1(101, 103)).pretty}")
  println(s"Part 2:\n ${runBenchmarked(Input.mainInput, part2(101, 103)).pretty}")
