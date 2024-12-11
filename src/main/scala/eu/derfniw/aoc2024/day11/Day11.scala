package eu.derfniw.aoc2024.day11

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

type StoneRow = List[Int]

def calculateLengthAfterBlinks(stoneRow: StoneRow, blinks: Int): Long =
  val memory = collection.mutable.Map.empty[(Int, Long), Long]
  def helper(stone: Long, blinksLeft: Int): Long =
    if memory.contains((blinksLeft, stone)) then memory((blinksLeft, stone))
    else if blinksLeft == 0 then 1
    else
      val result = stone match
        case 0 => helper(1, blinksLeft - 1)
        case n if n.toString.length % 2 == 0 =>
          val nStr          = n.toString
          val (left, right) = nStr.splitAt(nStr.length / 2)
          helper(left.toLong, blinksLeft - 1) + helper(right.toLong, blinksLeft - 1)
        case other => helper(other * 2024, blinksLeft - 1)
      memory((blinksLeft, stone)) = result
      result

  stoneRow.map(helper(_, blinks)).sum
end calculateLengthAfterBlinks

def parseInput(s: Seq[String]): StoneRow = s.head.split(" ").map(_.toInt).toList

def part1(input: Seq[String]): Long =
  val stoneRow = parseInput(input)
  calculateLengthAfterBlinks(stoneRow, 25)

def part2(input: Seq[String]): Long =
  val stoneRow = parseInput(input)
  calculateLengthAfterBlinks(stoneRow, 75)

object Inputs extends InputReader(11)

@main
def day11(): Unit =
  println(s"Part1:\n ${runBenchmarked(Inputs.mainInput, part1).pretty}")
  println(s"Part2:\n ${runBenchmarked(Inputs.mainInput, part2).pretty}")
