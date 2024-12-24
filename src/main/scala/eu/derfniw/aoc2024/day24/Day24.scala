package eu.derfniw.aoc2024.day24

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

enum Op(val left: String, val right: String):
  case And(l: String, r: String) extends Op(l, r)
  case Or(l: String, r: String)  extends Op(l, r)
  case Xor(l: String, r: String) extends Op(l, r)

  def name: String = this match
    case And(_, _) => "AND"
    case Or(_, _)  => "OR"
    case Xor(_, _) => "XOR"
end Op

class Circuit(val ops: Map[String, Op], val initialValues: Map[String, Boolean]):
  private val allWires    = ops.keySet ++ initialValues.keySet
  private val outputs     = allWires.filter(_.head == 'z')
  private val inputs      = allWires.filter(w => w.head == 'x' || w.head == 'y')
  private val knownValues = mutable.Map.from(initialValues)

  private def evalWire(wire: String): Boolean =
    knownValues.getOrElseUpdate(wire, evalOp(ops(wire)))

  private def evalOp(op: Op): Boolean = op match
    case Op.And(l, r) => evalWire(l) & evalWire(r)
    case Op.Or(l, r)  => evalWire(l) | evalWire(r)
    case Op.Xor(l, r) => evalWire(l) ^ evalWire(r)

  private def getNumber(prefix: Char): Long = allWires.toSeq
    .filter(_.head == prefix)
    .sorted(Ordering[String].reverse) // most significant bit first!.
    .map(evalWire)
    .foldLeft(0L)((acc, b) => (acc << 1) | (if b then 1 else 0))

  private def isDag: Boolean =
    def isDag(from: String, visited: Set[String] = Set.empty): Boolean =
      if visited(from) then false
      else
        val newVisited = visited + from
        ops.get(from).forall { op =>
          isDag(op.left, newVisited) && isDag(op.right, newVisited)
        }
    outputs.forall(isDag(_))
  end isDag

  lazy val zValue: Long = getNumber('z')

  // As the circuit should be a binary adder we can identify invalid links.
  // The following links are valid:
  //  zXX -> XOR <stuff>
  //  AND -> XOR | AND
  //  OR  -> AND
  def findInvalidLinks: Set[String] = ops.collect {
    case (wire, Op.And(_, _)) if outputs(wire)                          => wire
    case (wire, Op.Or(_, _)) if outputs(wire)                           => wire
    case (_, Op.Or(_, b)) if !ops(b).isInstanceOf[Op.And]               => b
    case (_, Op.Or(a, _)) if !ops(a).isInstanceOf[Op.And]               => a
    case (_, Op.Xor(_, b)) if !inputs(b) && ops(b).isInstanceOf[Op.And] => b
    case (_, Op.And(a, _)) if !inputs(a) && ops(a).isInstanceOf[Op.And] => a
  }.toSet - "z45" // exception, last output comes from "OR" as its carry.

end Circuit

object Circuit:

  def fromInput(input: Seq[String]): Circuit =
    val initialValues = input
      .takeWhile(_.nonEmpty)
      .collect { case s"$wire: $value" =>
        (wire, value == "1")
      }
      .toMap
    val ops = input
      .dropWhile(_.nonEmpty)
      .collect {
        case s"$l AND $r -> $wire" => (wire, Op.And(l, r))
        case s"$l OR $r -> $wire"  => (wire, Op.Or(l, r))
        case s"$l XOR $r -> $wire" => (wire, Op.Xor(l, r))
      }
      .toMap
    Circuit(ops, initialValues)
  end fromInput
end Circuit

def part1(input: Seq[String]): Long =
  Circuit.fromInput(input).zValue

def part2(input: Seq[String]): String =
  Circuit.fromInput(input).findInvalidLinks.toList.sorted.mkString(",")

object Input extends InputReader(24)

@main
def day24(): Unit =
  println(s"Part 1:\n ${runBenchmarked(Input.mainInput, part1).pretty}")
  println(s"Part 2:\n ${runBenchmarked(Input.mainInput, part2, 0).pretty}")
