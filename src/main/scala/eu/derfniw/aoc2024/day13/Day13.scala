package eu.derfniw.aoc2024.day13

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

case class Button(dx: Int, dy: Int)

object Button:
  private val lineParser = """Button [AB]: X\+(\d+), Y\+(\d+)""".r
  def fromLine(line: String): Button = line match
    case lineParser(dx, dy) => Button(dx.toInt, dy.toInt)

case class Game(a: Button, b: Button, priceX: Long, priceY: Long):

  def convertToTwo: Game =
    this.copy(priceX = priceX + 10000000000000L, priceY = priceY + 10000000000000L)

  def minCost(limit: Long): Option[Long] =
    val bPresses = (priceY * a.dx - priceX * a.dy) / (b.dy * a.dx - b.dx * a.dy)
    val aPresses = (priceX - bPresses * b.dx) / a.dx
    val derivedX = aPresses * a.dx + bPresses * b.dx
    val derivedY = aPresses * a.dy + bPresses * b.dy
    if derivedY == priceY && derivedX == priceX && aPresses <= limit && bPresses <= limit then
      Some(aPresses * Game.aPrice + bPresses * Game.bPrice)
    else None
  end minCost
end Game

object Game:
  private val aPrice     = 3
  private val bPrice     = 1
  private val gameParser = """Prize: X=(\d+), Y=(\d+)""".r
  def fromInput(input: Seq[String]): Seq[Game] =
    input.view
      .filter(_.nonEmpty)
      .grouped(3)
      .map { ls =>
        val lines   = ls.toSeq
        val buttonA = Button.fromLine(lines(0))
        val buttonB = Button.fromLine(lines(1))
        lines(2) match
          case gameParser(priceX, priceY) => Game(buttonA, buttonB, priceX.toLong, priceY.toLong)
      }
      .toSeq
end Game

def part1(input: Seq[String]): Long =
  val games    = Game.fromInput(input)
  val minCosts = games.map(_.minCost(100L))
  minCosts.flatten.sum

def part2(input: Seq[String]): Long =
  val games    = Game.fromInput(input)
  val twoGames = games.map(_.convertToTwo.minCost(Long.MaxValue))
  twoGames.flatten.sum

object Inputs extends InputReader(13)

@main
def day13(): Unit =
  println(s"Part1:\n ${runBenchmarked(Inputs.mainInput, part1).pretty}")
  println(s"Part2:\n ${runBenchmarked(Inputs.mainInput, part2).pretty}")
