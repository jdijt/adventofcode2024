package eu.derfniw.aoc2024.day05

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.annotation.tailrec

type Rule   = (Int, Int)
type Manual = IndexedSeq[Int]

def parseInput(input: Seq[String]): (Set[Rule], Seq[Manual]) =
  val emptyLine   = input.indexWhere(_.isEmpty)
  val ruleLines   = input.slice(0, emptyLine)
  val manualLines = input.slice(emptyLine + 1, input.length)

  val rules   = ruleLines.map { case s"$a|$b" => (a.toInt, b.toInt) }.toSet
  val manuals = manualLines.map(_.split(",").map(_.toInt).toIndexedSeq)
  (rules, manuals)
end parseInput

extension (m: Manual)
  def isCorrect(rules: Set[Rule]): Boolean =
    // This method assumes there are no duplicate pages
    m.combinations(2).forall { case Seq(a, b) => rules.contains(a -> b) }

  def middlePage: Int = m(m.length / 2)

  def repairMistake(rules: Set[Rule]): Manual =
    // As isCorrect, this assumes there are no duplicate pages
    val indexedPagePairs = m.zipWithIndex.combinations(2).map { case Seq(a, b) => a -> b }
    // Find the first mistake:
    // This method assumes that if "a -> b" rule is missing, then "b -> a" is not.
    indexedPagePairs.find { case ((x1, _), (x2, _)) => !rules.contains(x1 -> x2) } match
      case Some(((v1, i1), (v2, i2))) => m.updated(i1, v2).updated(i2, v1)
      case None                       => m

  @tailrec
  def repairAllMistakes(rules: Set[Rule]): Manual =
    val repaired = m.repairMistake(rules)
    if repaired == m then repaired
    else repaired.repairAllMistakes(rules)
end extension

def part1(input: Seq[String]): Int =
  val (rules, manuals) = parseInput(input)
  manuals.filter(_.isCorrect(rules)).map(_.middlePage).sum

def part2(input: Seq[String]): Int =
  val (rules, manuals) = parseInput(input)
  manuals
    .filterNot(_.isCorrect(rules))
    .map(_.repairAllMistakes(rules).middlePage)
    .sum

object Inputs extends InputReader(5)

@main
def day05(): Unit =
  println(s"Part1:\n ${runBenchmarked(Inputs.mainInput, part1).pretty}")
  println(s"Part2:\n ${runBenchmarked(Inputs.mainInput, part2).pretty}")
