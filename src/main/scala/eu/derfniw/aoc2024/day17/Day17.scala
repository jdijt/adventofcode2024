package eu.derfniw.aoc2024.day17

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.annotation.tailrec

extension (i: Long) def pow(j: Long): Long = math.pow(i, j).toLong

case class ProgramState(
    program: IndexedSeq[Int],
    ip: Int,
    a: Long,
    b: Long,
    c: Long,
    out: IndexedSeq[Int]
):
  @tailrec
  final def execute: ProgramState =
    if ip >= program.length then this
    else next.execute

  def setA(value: Long): ProgramState = copy(a = value)

  // Pseudocode of the test2 program:
  // 0: a = a >> 3      # 0, 3
  // 1: out += a % 8    # 5, 4
  // 2: jump to 0       # 3, 0 if a != 0
  //
  // Pseudocode of the input program:
  // 0: b = a % 8       # 2, 4
  // 1: b = b xor 101   # 1, 5
  // 2: c = a >> b      # 7, 5
  // 3: b = b xor 110   # 1, 6
  // 4: a = a >> 3      # 0, 3  the shift! (also present in sample program, test2.txt)
  // 5: b = b xor c     # 4, 6
  // 6: out += b % 8    # 5, 5
  // 7: jump to 0       # 3, 0  if a != 0
  // This is the source of the "shift 3" in the search, because for every loop:
  // - The least significant 3 bits of a are mixed into b/c to calculate the output value (step 0)
  // - The remainder is left behind for the next iteration (step 4).
  // - This is true for both the sample program and the input program.
  // Therefore, we can calculate the value backwards from the output 3 bits at a time.
  //  This limits the search space to 16 * 2^3 = 128 values.
  //  There are some dead ends here and we need to be able to backtrack,
  //  this is detected when more than 1 element of the result mismatches with the program.
  private def _findQuine(currentA: Long, matchLastN: Int): Option[Long] =
    val attempt         = setA(currentA).execute
    val relevantProgram = program.takeRight(matchLastN)

    if attempt.out.drop(1) != relevantProgram.drop(1) then None
    else if attempt.out == relevantProgram && matchLastN == program.length then Some(currentA)
    // Prefix match, attempt with a shifted 3 bits, or next a if that breaks down.
    else if attempt.out == relevantProgram then
      _findQuine(currentA << 3, matchLastN + 1).orElse(_findQuine(currentA + 1, matchLastN))
    else _findQuine(currentA + 1, matchLastN)
    end if
  end _findQuine

  def findQuine: Long = _findQuine(0, 1).get

  private def next: ProgramState =
    val opcode  = program(ip)
    val operand = program(ip + 1)
    opcode match
      case 0           => copy(ip = ip + 2, a = a >> readComboOperand(operand))
      case 1           => copy(ip = ip + 2, b = b ^ operand.toLong)
      case 2           => copy(ip = ip + 2, b = readComboOperand(operand) % 8)
      case 3 if a == 0 => copy(ip = ip + 2)
      case 3           => copy(ip = operand)
      case 4           => copy(ip = ip + 2, b = b ^ c)
      case 5           => copy(ip = ip + 2, out = out :+ (readComboOperand(operand) % 8).toInt)
      case 6           => copy(ip = ip + 2, b = a >> readComboOperand(operand))
      case 7           => copy(ip = ip + 2, c = a >> readComboOperand(operand))
    end match

  end next

  private def readComboOperand(operand: Int): Long =
    operand match
      case 4 => a
      case 5 => b
      case 6 => c
      case 7 => throw new IllegalArgumentException(s"Invalid combo operand: $operand")
      case _ => operand
end ProgramState

object ProgramState:
  def fromInput(input: Seq[String]): ProgramState = input match
    case s"Register A: $a" +: s"Register B: $b" +: s"Register C: $c" +: _ +: s"Program: $program" +: _ =>
      ProgramState(
        program.split(",").map(_.toInt).toIndexedSeq,
        0,
        a.toLong,
        b.toLong,
        c.toLong,
        IndexedSeq.empty
      )
    case _ => throw new IllegalArgumentException(s"Invalid input: $input")
end ProgramState

def part1(input: Seq[String]): String =
  ProgramState.fromInput(input).execute.out.mkString(",")

def part2(input: Seq[String]): Long =
  val program = ProgramState.fromInput(input)
  program.findQuine

object Inputs extends InputReader(17)

@main
def day17(): Unit =
  println(s"Part1: \n${runBenchmarked(Inputs.mainInput, part1).pretty}")
  println(s"Part2: \n${runBenchmarked(Inputs.mainInput, part2).pretty}")
