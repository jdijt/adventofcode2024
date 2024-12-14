package eu.derfniw.aoc2024.day03

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

enum Instruction:
  case Mul(a: Int, b: Int)
  case Do
  case Dont

case class ComputingState(acc: Int, mulEnabled: Boolean):
  def apply(instr: Instruction): ComputingState = instr match
    case Instruction.Mul(a, b) if this.mulEnabled => this.copy(acc = this.acc + (a * b))
    case Instruction.Do                           => this.copy(mulEnabled = true)
    case Instruction.Dont                         => this.copy(mulEnabled = false)
    case _                                        => this

object ComputingState:
  def initial: ComputingState = ComputingState(0, true)

def part1(input: Seq[String]): Int =
  val fullInput = input.mkString("\n")
  val mulRegex  = """mul\((\d+?),(\d+?)\)""".r
  mulRegex
    .findAllMatchIn(fullInput)
    .map { case mulRegex(a, b) => Instruction.Mul(a.toInt, b.toInt) }
    .foldLeft(ComputingState.initial)(_.apply(_))
    .acc

def part2(input: Seq[String]): Int =
  val fullInput  = input.mkString("\n")
  val instrRegex = """(mul\(\d+?,\d+?\)|do\(\)|don't\(\))""".r
  instrRegex
    .findAllMatchIn(fullInput)
    .map {
      case instrRegex(s"mul($a,$b)") => Instruction.Mul(a.toInt, b.toInt)
      case instrRegex("do()")        => Instruction.Do
      case instrRegex("don't()")     => Instruction.Dont
    }
    .foldLeft(ComputingState.initial)(_.apply(_))
    .acc
end part2

object Inputs extends InputReader(3)

@main
def day03(): Unit =
  println(s"Part 1: \n${runBenchmarked(Inputs.mainInput, part1).pretty}")
  println(s"Part 2: \n${runBenchmarked(Inputs.mainInput, part2).pretty}")
