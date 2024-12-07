package eu.derfniw.aoc2024.day07

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

case class Equation(solution: Long, expression: List[Long]):

  def isSolvable: Boolean =
    def helper(expression: List[Long]): Boolean = expression match
      case Nil                    => false
      case x :: _ if x > solution => false
      case x :: Nil               => x == solution
      case x :: y :: xs           => helper((x + y) :: xs) || helper((x * y) :: xs)

    helper(expression)

  def isSolvable2: Boolean =
    def helper(expression: List[Long]): Boolean = expression match
      case Nil                    => false
      case x :: _ if x > solution => false
      case x :: Nil               => x == solution
      case x :: y :: xs =>
        helper((x + y) :: xs)
        || helper((x * y) :: xs)
        || helper((x.toString + y.toString).toLong :: xs)

    helper(expression)
  end isSolvable2
end Equation

object Equation:
  private val lineRegex = """(\d+): ((?:\d+ ?)+)""".r
  def parse(input: String): Equation = input match
    case lineRegex(solution, expression) =>
      Equation(solution.toLong, expression.split(" ").map(_.toLong).toList)
    case _ => throw IllegalArgumentException(s"Invalid input line: $input")

def part1(input: Seq[String]): Long =
  input.view.map(Equation.parse).filter(_.isSolvable).map(_.solution).sum

def part2(input: Seq[String]): Long =
  input.view.map(Equation.parse).filter(_.isSolvable2).map(_.solution).sum

object Inputs extends InputReader(7)

@main
def day07: Unit =
  println(s"Part1: ${runBenchmarked(Inputs.mainInput, part1).pretty}")
  println(s"Part1: ${runBenchmarked(Inputs.mainInput, part2).pretty}")
