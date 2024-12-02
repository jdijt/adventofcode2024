package eu.derfniw.aoc2024.day02

import eu.derfniw.aoc2024.util.InputReader

import scala.math.abs

object Inputs extends InputReader(2)

enum Direction:
  case Incr, Decr, Eq

def isReportSafe(report: Seq[Int]): Boolean =
  val steps = report
    .sliding(2)
    .map { case Seq(a, b) =>
      val diff = a - b
      val direction = diff match
        case 0          => Direction.Eq
        case x if x > 0 => Direction.Decr
        case x if x < 0 => Direction.Incr
      (direction, abs(diff))
    }
    .toSeq
  val firstDirection = steps.head._1
  steps.forall { case (direction, diff) => direction == firstDirection && diff >= 1 && diff <= 3 }
end isReportSafe

def part1(input: Seq[String]): Int =
  val parsed = input.map(_.split(" ").map(_.toInt).toSeq)
  parsed.count(isReportSafe)

def part2(input: Seq[String]): Int =
  val parsed = input.map(_.split(" ").map(_.toInt).toSeq)
  parsed.count(report =>
    val alternatives = report.indices.map(idx => report.patch(idx, Nil, 1)) :+ report
    alternatives.exists(isReportSafe)
  )
end part2

@main
def day02(): Unit =
  println(s"Part 1: ${part1(Inputs.mainInput)}")
  println(s"Part 2: ${part2(Inputs.mainInput)}")
