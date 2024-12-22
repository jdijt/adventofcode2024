package eu.derfniw.aoc2024.day22

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

type SecretNumber = Long

extension (n: SecretNumber)
  @tailrec
  def calculateNextSecretNumber(iterations: Int): SecretNumber =
    if iterations == 0 then n
    else calculateNextSecretNumber.calculateNextSecretNumber(iterations - 1)

  def calculateNextSecretNumber: SecretNumber =
    val step1 = (n ^ (n * 64))         % 16777216
    val step2 = (step1 ^ (step1 / 32)) % 16777216
    (step2 ^ (step2 * 2048)) % 16777216

  // Creates a map of the first occurrence of a certain four-sequence of diffs
  // & the amount of bananas that is worth.
  def toPriceMap(iterations: Int): Map[Seq[Int], Int] =
    val lastDiffs = mutable.ArrayDeque.empty[Int]
    val priceMap = mutable.Map.empty[Seq[Int], Int]
    var current = n
    var currentPrice = n % 10
    for _ <- 0 until iterations do
      val next = current.calculateNextSecretNumber
      val nextPrice = next % 10
      val diff = (nextPrice - currentPrice).toInt
      lastDiffs.append(diff)
      if lastDiffs.size == 4 then
        val diffs = lastDiffs.toList
        if !priceMap.contains(diffs) then priceMap(diffs) = nextPrice.toInt
        lastDiffs.removeHead()
      current = next
      currentPrice = nextPrice

    priceMap.filter { case (_, v) => v > 0}.toMap
end extension

def parseInput(input: Seq[String]): Seq[SecretNumber] = input.map(_.toInt)

def part1(input: Seq[String]): Long =
  parseInput(input).map(_.calculateNextSecretNumber(2000)).sum

def part2(input: Seq[String]): Int =
  val secretNumbers = parseInput(input)
  val priceMaps = secretNumbers.map(_.toPriceMap(2000))
  val allSequences = priceMaps.map(_.keySet).reduce(_ ++ _)
  allSequences.par.map {seq => priceMaps.flatMap(_.get(seq)).sum}.max

object Input extends InputReader(22)

@main
def day22(): Unit =
  println(s"Part1: \n${runBenchmarked(Input.mainInput, part1).pretty}")
  println(s"Part2: \n${runBenchmarked(Input.mainInput, part2, 5).pretty}")
