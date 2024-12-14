package eu.derfniw.aoc2024.day01

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.math.abs

object Inputs extends InputReader(1)

val pairRegex = """(\d+) {3}(\d+)""".r
def parseInput(input: Seq[String]): (Seq[Int], Seq[Int]) =
  input
    .collect { case pairRegex(left, right) => (left.toInt, right.toInt) }
    .foldLeft((Seq.empty, Seq.empty)) { case ((seq1, seq2), (left, right)) =>
      (seq1 :+ left, seq2 :+ right)
    }

def part1(input: Seq[String]): Int =
  val (seq1, seq2) = parseInput(input)
  seq1.sorted.zip(seq2.sorted).map((n1, n2) => abs(n1 - n2)).sum

def part2(input: Seq[String]): Int =
  val (seq1, seq2) = parseInput(input)
  val counts       = seq2.groupBy(identity).view.mapValues(_.size).toMap

  seq1.map(n => n * counts.getOrElse(n, 0)).sum

@main
def day01(): Unit =
  println(s"Part 1: \n${runBenchmarked(Inputs.mainInput, part1).pretty}")
  println(s"Part 2: \n${runBenchmarked(Inputs.mainInput, part2).pretty}")
