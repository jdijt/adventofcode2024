package eu.derfniw.aoc2024.day19

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

type Design = String

case class Towel(pattern: Design):
  // Note; Get away with recursive solutions here because depth will be limited
  // Given max length of < 200 characters for the patterns to test.
  def canBeMadeWith(designs: Seq[Design]): Boolean =
    val memory = mutable.Map.empty[String, Boolean]
    def _canBeMadeWith(remainder: String): Boolean =
      memory.getOrElseUpdate(
        remainder,
        designs.exists { p =>
          if p == remainder then true
          else if p.length > remainder.length then false
          else if remainder.startsWith(p) then _canBeMadeWith(remainder.drop(p.length))
          else false
        }
      )
    end _canBeMadeWith
    _canBeMadeWith(pattern)
  end canBeMadeWith

  def countDesignOptions(designs: Seq[Design]): Long =
    val memory = mutable.Map.empty[String, Long]
    def _countDesignOptions(remainder: String): Long =
      memory.getOrElseUpdate(
        remainder,
        designs.map { p =>
          if p == remainder then 1
          else if p.length > remainder.length then 0
          else if remainder.startsWith(p) then _countDesignOptions(remainder.drop(p.length))
          else 0
        }.sum
      )
    _countDesignOptions(pattern)
  end countDesignOptions
end Towel

object Towel:
  def fromString(s: String): Towel = Towel(s)

def parseInput(input: Seq[String]): (Seq[Design], Seq[Towel]) =
  val parts   = input.head.split(", ").toList
  val designs = input.drop(2).map(Towel.fromString)
  (parts, designs)

def part1(input: Seq[String]): Int =
  val (designs, towels) = parseInput(input)
  towels.count(_.canBeMadeWith(designs))

def part2(input: Seq[String]): Long =
  val (designs, towels) = parseInput(input)
  towels.par.map(_.countDesignOptions(designs)).sum

object Input extends InputReader(19)

@main
def day19(): Unit =
  println(s"Part 1: \n${runBenchmarked(Input.mainInput, part1).pretty}")
  println(s"Part 2: \n${runBenchmarked(Input.mainInput, part2).pretty}")
