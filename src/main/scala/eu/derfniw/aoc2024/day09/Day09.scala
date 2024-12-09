package eu.derfniw.aoc2024.day09

import eu.derfniw.aoc2024.day09.FsContent.{File, Free}
import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.annotation.tailrec

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

  def fileById(fileId: Int): (FsContent.File, Int) =
    c.zipWithIndex
      .find {
        case (FsContent.File(_, id), _) => id == fileId
        case _                          => false
      }
      .get
      .asInstanceOf[(FsContent.File, Int)]

  def findFreeBlock(minSize: Int, maxIndex: Int): Option[(FsContent.Free, Int)] =
    c.zipWithIndex
      .filter(_._2 < maxIndex)
      .find {
        case (FsContent.Free(size), _) => size >= minSize
        case _                         => false
      }
      .asInstanceOf[Option[(FsContent.Free, Int)]]

  @tailrec
  private def _deFragment(fileId: Int): CompactFs =
    if fileId < 0 then c
    else
      val (file, fileLocation) = c.fileById(fileId)

      val newFs = findFreeBlock(file.size, fileLocation) match
        case Some((freeBlock, freeBlockLocation)) =>
          val newFilePatch =
            if file.size < freeBlock.size then Seq(file, FsContent.Free(freeBlock.size - file.size))
            else Seq(file)
          c.updated(fileLocation, FsContent.Free(file.size))
            .patch(freeBlockLocation, newFilePatch, 1)

        case _ => c
      newFs._deFragment(fileId - 1)

  def deFragment(): CompactFs =
    val lastFileId = c.lastFileId
    c._deFragment(lastFileId)
end extension

extension (c: ExpandedFs)
  @tailrec
  private def _compactBlocks(leftCursor: Int, rightCursor: Int): ExpandedFs =
    val firstEmptyBlock = c.indexWhere(_ == BlockContent.Empty, leftCursor)
    val lastFileBlock   = c.lastIndexWhere(_ != BlockContent.Empty, rightCursor)
    if firstEmptyBlock > lastFileBlock then c
    else
      c.updated(firstEmptyBlock, c(lastFileBlock))
        .updated(lastFileBlock, BlockContent.Empty)
        ._compactBlocks(firstEmptyBlock + 1, lastFileBlock - 1)

  def compactBlocks(): ExpandedFs = _compactBlocks(0, c.length - 1)

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
