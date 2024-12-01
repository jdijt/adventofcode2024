package eu.derfniw.aoc2024.day01

import eu.derfniw.aoc2024.util.InputReader

class Day01Test extends munit.FunSuite, InputReader(1):
  private lazy val input1 = readInputFile("test1.txt").get

  test("part1") {
    val result = part1(input1)
    assertEquals(result, 11)
  }

  test("part2") {
    val result = part2(input1)
    assertEquals(result, 31)
  }
end Day01Test
