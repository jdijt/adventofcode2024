package eu.derfniw.aoc2024.day04

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.util.Try

extension (g: Seq[String])
  def wordsFrom(x: Int, y: Int, length: Int): Seq[String] =
    // Take subsequences in the following direction from the grid:
    //  right left up down upright upleft downright
    val words = Seq(
      Try(g.slice(y, y + length).map(_.charAt(x)).mkString),
      Try(g.slice(y - length + 1, y + 1).map(_.charAt(x)).mkString.reverse),
      Try(g(y).slice(x - length + 1, x + 1).mkString.reverse),
      Try(g(y).slice(x, x + length).mkString),
      Try(
        g.slice(y - length + 1, y + 1)
          .zipWithIndex
          .map((row, idx) => row.charAt(x + (3 - idx)))
          .mkString
          .reverse
      ),
      Try(
        g.slice(y - length + 1, y + 1)
          .zipWithIndex
          .map((row, idx) => row.charAt(x - (3 - idx)))
          .mkString
          .reverse
      ),
      Try(g.slice(y, y + length).zipWithIndex.map((row, idx) => row.charAt(x + idx)).mkString),
      Try(
        g.slice(y, y + length).zipWithIndex.map((row, idx) => row.charAt(x - idx)).mkString
      )
    ).flatMap(_.toOption).filter(_.length == length)
    words

  def getWord(startX: Int, startY: Int, endX: Int, endY: Int): Option[String] =


def part1(input: Seq[String]): Int =
  input.indices
    .flatMap(y => input(y).indices.flatMap(x => input.wordsFrom(x, y, 4)))
    .count(_ == "XMAS")

def part2(input: Seq[String]): Int = ???

object Inputs extends InputReader(4)

@main
def day05 =
  println(s"Part1:\n ${runBenchmarked(Inputs.mainInput, part1).pretty}")
  // println(s"Part2:\n ${runBenchmarked(Inputs.mainInput, part2).pretty}")
