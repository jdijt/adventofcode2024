package eu.derfniw.aoc2024.day25

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.{View, mutable}

case class Key(code: Seq[Int]):
  def fits(lock: Lock): Boolean = code.zip(lock.code).forall((key, lock) => key + lock <= 5)

case class Lock(code: Seq[Int])

object Input extends InputReader(25):

  def parseInput(input: Seq[String]): (Seq[Key], Seq[Lock]) =
    val keys  = mutable.ArrayBuffer[Key]()
    val locks = mutable.ArrayBuffer[Lock]()

    val groups = View.unfold(input.dropWhile(_.isEmpty)) { lines =>
      if lines.isEmpty then None
      else
        val group = lines.takeWhile(_.nonEmpty)
        val rest  = lines.drop(group.length).dropWhile(_.isEmpty)
        Some((group, rest))
    }

    for group <- groups do
      val counts = group.slice(1, group.length - 1).transpose.map(_.count(_ == '#'))
      if group.head.count(_ == '#') == 5 then locks += Lock(counts)
      else if group.last.count(_ == '#') == 5 then keys += Key(counts)
      else throw new IllegalArgumentException("Invalid input, neither key nor lock")

    (keys.toSeq, locks.toSeq)
  end parseInput
end Input

def part1(input: Seq[String]): Int =
  val (keys, locks) = Input.parseInput(input)
  locks.map(l => keys.count(_.fits(l))).sum

@main
def day25(): Unit =
  println(s"Part 1: \n${runBenchmarked(Input.mainInput, part1).pretty}")