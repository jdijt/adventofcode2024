package eu.derfniw.aoc2024.day21

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

case class Point(x: Int, y: Int):
  def distanceTo(other: Point): Int = math.abs(x - other.x) + math.abs(y - other.y)

enum Move:
  case Up, Down, Left, Right, Activate

extension [T](pad: Seq[Seq[Option[T]]])
  def routesFrom(v: T): Map[(T, T), Seq[Move]] =


object KeyPad:
  private val keyPad = List(
    List(Some('7'), Some('8'), Some('9')),
    List(Some('4'), Some('5'), Some('6')),
    List(Some('1'), Some('2'), Some('3')),
    List(None,      Some('0'), Some('A'))
  )

  lazy val routes: Map[(Char, Char), Seq[Move]] = ???

end KeyPad

object DirPad:
  private val dirPad = List(
    List(None,      Some('^'), Some('A')),
    List(Some('<'), Some('v'), Some('>'))
  )
end DirPad

def part1(input: Seq[String]): Int = ???

def part2(input: Seq[String]): Int = ???

object Input extends InputReader(21)

@main
def day22(): Unit =
  println(s"Part1: \n${runBenchmarked(Input.mainInput, part1).pretty}")
