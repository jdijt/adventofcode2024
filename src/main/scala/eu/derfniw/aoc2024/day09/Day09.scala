package eu.derfniw.aoc2024.day09

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.mutable

def part1(input: Seq[String]): Long =
  val expandedFs = mutable.ArrayDeque.from(input.head.zipWithIndex.flatMap {
    // Free space
    case (c, i) if i % 2 == 1 => Seq.fill(c.asDigit)(-1)
    // File
    case (c, i) => Seq.fill(c.asDigit)(i / 2)
  })
  val result = mutable.ArrayDeque.empty[Int]
  while expandedFs.nonEmpty do
    val next = expandedFs.head
    if next == -1 then
      val nextEnd = expandedFs.last
      // Front and back are empty space, advance rear to next block
      if nextEnd == -1 then expandedFs.removeLast()
      // Move block from rear to front.
      else
        result += nextEnd
        expandedFs.removeLast()
        expandedFs.removeHead()
    // Keep front block.
    else
      result += next
      expandedFs.removeHead()
    end if
  end while

  result.zipWithIndex.foldLeft(0L) { case (acc, (file, i)) =>
    acc + (file * i)
  }
end part1

enum Content:
  case Free(size: Int)
  case File(size: Int, id: Int)

object Input extends InputReader(9)

@main
def day09(): Unit =
  println(s"Part 1:\n ${runBenchmarked(Input.mainInput, part1).pretty}")
