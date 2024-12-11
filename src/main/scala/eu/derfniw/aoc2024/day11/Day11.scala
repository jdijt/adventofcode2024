package eu.derfniw.aoc2024.day11

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

class StoneRow(val row: List[Long]):
  def blink(): StoneRow =
    val newRow = row.flatMap {
      case 0 => List(1L)
      case n if n.toString.length % 2 == 0 =>
        val (left, right) = n.toString.splitAt(n.toString.length / 2)
        List(left.toLong, right.toLong)
      case other => List(other * 2024L)
    }
    StoneRow(newRow)
end StoneRow

object StoneRow:
  def fromInput(s: Seq[String]): StoneRow = StoneRow(s.head.split(" ").map(_.toLong).toList)

def part1(input: Seq[String]): Int =
  (1 to 25).foldLeft(StoneRow.fromInput(input))((acc, _) => acc.blink()).row.size

def part2(input: Seq[String]): Int =
  (1 to 75).foldLeft(StoneRow.fromInput(input))((acc, _) => acc.blink()).row.size

object Inputs extends InputReader(11)

@main
def day11(): Unit =
  println(s"Part1:\n ${runBenchmarked(Inputs.mainInput, part1).pretty}")
  println(s"Part1:\n Result: ${part2(Inputs.mainInput)}")
