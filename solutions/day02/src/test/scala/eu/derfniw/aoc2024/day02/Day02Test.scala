package eu.derfniw.aoc2024.day02

import eu.derfniw.aoc2024.util.InputReader

class Day02Test extends munit.FunSuite, InputReader(2):
  private lazy val input1 = readInputFile("test1.txt").get

  test("part1") {
    val result = part1(input1)
    assertEquals(result, 2)
  }

  test("part2") {
    val result = part2(input1)
    assertEquals(result, 4)
  }

end Day02Test
