package eu.derfniw.aoc2024.day04

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.util.Try

extension (g: Seq[String])
  def countXmasFrom(x: Int, y: Int): Int =
    Seq(
      // Straight down, right
      Try(g.slice(y, y + 4).map(_.charAt(x)).mkString),
      Try(g(y).slice(x, x + 4).mkString),
      // Straight up, left
      // Here we read backwards, so we need to offset the index by 1
      // Otherwise we skip the char at (x,y)
      Try(g.slice(y - 3, y + 1).map(_.charAt(x)).mkString.reverse),
      Try(g(y).slice(x - 3, x + 1).mkString.reverse),
      // Diagonals: down-right, down-left
      Try(g.slice(y, y + 4).zipWithIndex.map((row, idx) => row.charAt(x + idx)).mkString),
      Try(g.slice(y, y + 4).zipWithIndex.map((row, idx) => row.charAt(x - idx)).mkString),
      // Diagonals: up-right, up-left
      // In the following cases we read in reverse again, so these
      // Similar reason as above, we need to offset the index by 1 due to reversal,
      // This is also why '3-idx' for x, instead of 4.
      Try(g.slice(y - 3, y + 1).zipWithIndex.map((row, idx) => row.charAt(x + (3 - idx))).mkString.reverse),
      Try(g.slice(y - 3, y + 1).zipWithIndex.map((row, idx) => row.charAt(x - (3 - idx))).mkString.reverse)
    ).flatMap(_.toOption).count(w => w == "XMAS")

  def isCross(x: Int, y: Int): Boolean = Try {
    if g(y)(x) != 'A' then false
    else
      // \ == MAS | SAM
      (g(y - 1)(x - 1) == 'M' && g(y + 1)(x + 1) == 'S' || g(y - 1)(x - 1) == 'S' && g(y + 1)(x + 1) == 'M')
      // / == MAS | SAM
      && (g(y + 1)(x - 1) == 'M' && g(y - 1)(x + 1) == 'S' || g(y + 1)(x - 1) == 'S' && g(y - 1)(x + 1) == 'M')
  }.getOrElse(false)
end extension

def part1(input: Seq[String]): Int =
  val indexedInput = input.toIndexedSeq
  indexedInput.indices
    .flatMap(y => input(y).indices.map(x => input.countXmasFrom(x, y)))
    .sum

def part2(input: Seq[String]): Int =
  val indexedInput = input.toIndexedSeq
  indexedInput.indices
    .map(y => input(y).indices.count(x => input.isCross(x, y)))
    .sum
object Inputs extends InputReader(4)

@main
def day05(): Unit =
  println(s"Part1:\n ${runBenchmarked(Inputs.mainInput, part1).pretty}")
  println(s"Part2:\n ${runBenchmarked(Inputs.mainInput, part2).pretty}")
