package eu.derfniw.aoc2024.day11

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

class StoneRow(row: List[Int]):
  def calculateLengthAfterBlinks(blinks: Int): Long =
    val memory = collection.mutable.Map.empty[(Int, Long), Long]
    def helper(stone: Long, blinks: Int): Long =
      if memory.contains((blinks, stone)) then memory((blinks, stone))
      else if blinks == 0 then 1
      else
        val result = stone match
          case 0 => helper(1, blinks - 1)
          case n if n.toString.length % 2 == 0 =>
            val nStr          = n.toString
            val (left, right) = nStr.splitAt(nStr.length / 2)
            helper(left.toLong, blinks - 1) + helper(right.toLong, blinks - 1)
          case other => helper(other * 2024, blinks - 1)
        memory((blinks, stone)) = result
        result
    row.map(helper(_, blinks)).sum
  end calculateLengthAfterBlinks
end StoneRow

object StoneRow:
  def fromInput(input: Seq[String]): StoneRow = StoneRow(input.head.split(" ").map(_.toInt).toList)

def part1(input: Seq[String]): Long =
  val stoneRow = StoneRow.fromInput(input)
  stoneRow.calculateLengthAfterBlinks(25)

def part2(input: Seq[String]): Long =
  val stoneRow = StoneRow.fromInput(input)
  stoneRow.calculateLengthAfterBlinks(75)

object Inputs extends InputReader(11)

@main
def day11(): Unit =
  println(s"Part1:\n ${runBenchmarked(Inputs.mainInput, part1).pretty}")
  println(s"Part2:\n ${runBenchmarked(Inputs.mainInput, part2).pretty}")
