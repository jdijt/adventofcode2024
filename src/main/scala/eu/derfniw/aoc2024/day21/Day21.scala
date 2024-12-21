package eu.derfniw.aoc2024.day21

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

type Route = String
extension (r: Route)
  def revertRoute: Route = r.reverse.collect {
    case '^' => 'v'
    case 'v' => '^'
    case '<' => '>'
    case '>' => '<'
  }

  def keyPadToDirPad: Route =
    def _keyPadToDirPad(remainder: Route): Route =
      if remainder.size <= 1 then ""
      else
          val (a, b) = (remainder.head, remainder.tail.head)
          if a == b then "A" + _keyPadToDirPad(remainder.tail)
          else KeyPad.routes((a, b)) + "A" + _keyPadToDirPad(remainder.tail)
    _keyPadToDirPad("A" + r)

  def dirPadToDirPad: Route =
    def _dirPadToDirPad(remainder: Route): Route =
      if remainder.size <= 1 then ""
      else
          val (a, b) = (remainder.head, remainder.tail.head)
          if a == b then "A" + _dirPadToDirPad(remainder.tail)
          else DirPad.routes((a, b)) + "A" + _dirPadToDirPad(remainder.tail)
    _dirPadToDirPad("A" + r)
end extension

object KeyPad:
  // Predefined first half of the routes, this is small enough to manage by hand.
  // Reference:
  //   +---+---+---+
  //   | 7 | 8 | 9 |
  //   +---+---+---+
  //   | 4 | 5 | 6 |
  //   +---+---+---+
  //   | 1 | 2 | 3 |
  //   +---+---+---+
  //       | 0 | A |
  //       +---+---+
  // format: off
  private val baseRoutes: Map[(Char, Char), Route] = Map(
    ('A', '0') -> "<",    ('A', '1') -> "^<<",  ('A', '2') -> "<^",   ('A', '3') -> "^",
    ('A', '4') -> "<<^^", ('A', '5') -> "<^^",  ('A', '6') -> "^^",   ('A', '7') -> "^^^<<",
    ('A', '8') -> "<^^^", ('A', '9') -> "^^^",
    ('0', '1') -> "^<",   ('0', '2') -> "^",    ('0', '3') -> ">^",   ('0', '4') -> "^^<",
    ('0', '5') -> "^^",   ('0', '6') -> ">^^",  ('0', '7') -> "^^^<", ('0', '8') -> "^^^",
    ('0', '9') -> ">^^^",
    ('1', '2') -> ">",    ('1', '3') -> ">>",   ('1', '4') -> "^",    ('1', '5') -> ">^",
    ('1', '6') -> ">>^",  ('1', '7') -> "^^",   ('1', '8') -> ">^^",  ('1', '9') -> ">>^^",
    ('2', '3') -> ">",    ('2', '4') -> "<^",   ('2', '5') -> "^",    ('2', '6') -> ">^",
    ('2', '7') -> "<^^",  ('2', '8') -> "^^",   ('2', '9') -> ">^^",
    ('3', '4') -> "<<^",  ('3', '5') -> "<^",   ('3', '6') -> "^",    ('3', '7') -> "<<^^",
    ('3', '8') -> "<^^",  ('3', '9') -> "^^",
    ('4', '5') -> ">",    ('4', '6') -> ">>",   ('4', '7') -> "^",    ('4', '8') -> "<^",
    ('4', '9') -> ">>^",
    ('5', '6') -> ">",    ('5', '7') -> "<^",   ('5', '8') -> "^",    ('5', '9') -> ">^",
    ('6', '7') -> "<<^",  ('6', '8') -> "<^",   ('6', '9') -> "^",
    ('7', '8') -> ">",    ('7', '9') -> ">>",
    ('8', '9') -> ">"
  )
  // format: on
  val routes: Map[(Char, Char), Route] = baseRoutes ++ baseRoutes.map { case (k, v) =>
    (k.swap, v.revertRoute)
  }

end KeyPad

object DirPad:
  // Predefined first half of the routes, this is small enough to manage by hand.
  // Reference:
  //   +---+---+---+
  //   |   | ^ | A |
  //   +---+---+---+
  //   | < | v | > |
  //   +---+---+---+
  // format: off
  private val baseRoutes: Map[(Char, Char), Route] = Map(
    ('A', '^') -> "<",  ('A', 'v') -> "<v", ('A', '<') -> "v<<", ('A', '>') -> "v",
    ('^', '<') -> "v<", ('^', 'v') -> "v",  ('^', '>') -> ">v",
    ('>', '<') -> "<<", ('>', 'v') -> "<",
    ('v', '<') -> "<",
  )
  // format: on
  val routes: Map[(Char, Char), Route] = baseRoutes ++ baseRoutes.map { case (k, v) =>
    (k.swap, v.revertRoute)
  }

end DirPad

def part1(input: Seq[String]): Int =
  input.map { in =>
    val numeric = in.take(3)
    val route = in.keyPadToDirPad.dirPadToDirPad.dirPadToDirPad
    numeric.toInt * route.size
  }.sum

def part2(input: Seq[String]): Int =
  input.map { in =>
    val numeric = in.take(3)
    val route = in.keyPadToDirPad.dirPadToDirPad.dirPadToDirPad.dirPadToDirPad.dirPadToDirPad
      .dirPadToDirPad.dirPadToDirPad.dirPadToDirPad.dirPadToDirPad.dirPadToDirPad
      .dirPadToDirPad.dirPadToDirPad.dirPadToDirPad.dirPadToDirPad.dirPadToDirPad
      .dirPadToDirPad.dirPadToDirPad.dirPadToDirPad.dirPadToDirPad.dirPadToDirPad
      .dirPadToDirPad.dirPadToDirPad.dirPadToDirPad.dirPadToDirPad.dirPadToDirPad
    numeric.toInt * route.size
  }.sum

object Input extends InputReader(21)

@main
def day21(): Unit =
  println(s"Part1: \n${runBenchmarked(Input.mainInput, part1).pretty}")
  println(s"Part2: \n${runBenchmarked(Input.mainInput, part2).pretty}")
