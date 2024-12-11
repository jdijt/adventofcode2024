package eu.derfniw.aoc2024.day09

import eu.derfniw.aoc2024.day09.FsContent.{File, Free}
import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.mutable

type CompactFs  = IndexedSeq[FsContent]
type ExpandedFs = IndexedSeq[BlockContent]

enum FsContent:
  case Free(size: Int)
  case File(size: Int, id: Int)

enum BlockContent:
  case Empty
  case File(id: Int)

def parseInput(input: Seq[String]): CompactFs =
  input.head.zipWithIndex.map {
    case (c, i) if i % 2 == 1 => FsContent.Free(c.asDigit)
    case (c, i) => FsContent.File(c.asDigit, i / 2)
  }

extension (c: CompactFs)
  def expand(): ExpandedFs = c.flatMap {
    case FsContent.Free(size)     => Seq.fill(size)(BlockContent.Empty)
    case FsContent.File(size, id) => Seq.fill(size)(BlockContent.File(id))
  }

  def lastFileId: Int = c.findLast(_.isInstanceOf[FsContent.File]).get match
    case FsContent.File(_, id) => id
    case _                     => throw new RuntimeException("Unexpected empty")

  def deFragment(): CompactFs =
    val mutableFs = mutable.ArrayDeque.from(c)
    for x <- c.lastFileId to 0 by -1 do
      val fileLocation = mutableFs.lastIndexWhere {
        case FsContent.File(_, id) if id == x => true
        case _                                => false
      }
      val file = mutableFs(fileLocation).asInstanceOf[FsContent.File]
      val freeLocation = mutableFs.slice(0, fileLocation).indexWhere {
        case FsContent.Free(size) if size >= file.size => true
        case _                                         => false
      }
      if freeLocation != -1 then
        val freeBlock = mutableFs(freeLocation).asInstanceOf[FsContent.Free]
        val newFilePatch =
          if file.size < freeBlock.size then Seq(file, FsContent.Free(freeBlock.size - file.size))
          else Seq(file)
        mutableFs.update(fileLocation, FsContent.Free(file.size))
        mutableFs.patchInPlace(freeLocation, newFilePatch, 1)
      end if
    end for
    mutableFs.toIndexedSeq
  end deFragment
end extension

extension (c: ExpandedFs)
  def compactBlocks(): ExpandedFs =
    val mutableFs   = mutable.IndexedSeq.from(c)
    var leftCursor  = 0
    var rightCursor = c.length - 1
    while leftCursor < rightCursor do
      (mutableFs(leftCursor), mutableFs(rightCursor)) match
        case (BlockContent.Empty, BlockContent.Empty) =>
          rightCursor -= 1
        case (BlockContent.Empty, BlockContent.File(_)) =>
          mutableFs(leftCursor) = mutableFs(rightCursor)
          mutableFs(rightCursor) = BlockContent.Empty
          leftCursor += 1
          rightCursor -= 1
        case (BlockContent.File(_), _) =>
          leftCursor += 1
      end match
    end while
    mutableFs.toIndexedSeq
  end compactBlocks

  def checkSum(): Long =
    c.zipWithIndex.foldLeft(0L) {
      case (acc, (BlockContent.File(id), i)) => acc + (id * i)
      case (acc, (BlockContent.Empty, _))    => acc
    }
end extension

def part1(input: Seq[String]): Long =
  parseInput(input).expand().compactBlocks().checkSum()

def part2(input: Seq[String]): Long =
  parseInput(input).deFragment().expand().checkSum()

object Input extends InputReader(9)

@main
def day09(): Unit =
  println(s"Part 1:\n ${runBenchmarked(Input.mainInput, part1).pretty}")
  println(s"Part 2:\n ${runBenchmarked(Input.mainInput, part2).pretty}")
