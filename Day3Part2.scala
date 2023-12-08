import scala.collection.mutable

object Day3Part2 {
  case class Point(row: Int, col: Int)

  case class Part(num: Int, row: Int, colsLow: Int, colsHigh: Int /* exclusive */) {
    def coverage: Seq[Point] = (colsLow until colsHigh).toSeq.map { col =>
      Point(row, col)
    }
  }

  def parseLine(row: Int, col: Int, remaining: Seq[Char], colsLow: Int, acc: Seq[Char]): Seq[Part] = {
    if (remaining.isEmpty) {
      if (acc.nonEmpty) {
        Seq(Part(acc.mkString.toInt, row, colsLow, col))
      } else {
        Seq.empty
      }
    } else {
      if (remaining.head.isDigit) {
        if (acc.isEmpty)
          parseLine(row, col + 1, remaining.tail, col, Seq(remaining.head))
        else
          parseLine(row, col + 1, remaining.tail, colsLow, acc :+ remaining.head)
      } else {
        if (acc.nonEmpty) {
          Part(acc.mkString.toInt, row, colsLow, col) +: parseLine(row, col + 1, remaining.tail, colsLow, Seq.empty)
        } else {
          parseLine(row, col + 1, remaining.tail, colsLow, acc)
        }
      }
    }
  }

  def parseParts(input: Seq[String]): Seq[Part] = {
    input.zipWithIndex flatMap { case (line, row) =>
      parseLine(row, 0, line.toSeq, 0, Seq.empty)
    }
  }

  type PartMap = Map[Point, Part]

  def buildPartMap(parts: Seq[Part]): PartMap =
    parts.flatMap { part =>
      part.coverage.map(_ -> part)
    }.toMap

  def adjacency(point: Point, width: Int, height: Int): Seq[Point] =
    for {
      dx <- Seq(-1, 0, 1) if point.col > 0 && point.col < (width - 1)
      dy <- Seq(-1, 0, 1) if point.row > 0 && point.row < (height - 1)
    } yield Point(point.row + dx, point.col + dy)

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("day-3-input.txt", "UTF-8").getLines().toSeq
    val width = input.head.size
    val height = input.size

    val parts: Seq[Part] = parseParts(input)
    println(parts.mkString("\n"))

    val partMap = buildPartMap(parts)

    val gearRatios = input.zipWithIndex.flatMap { case (line, row) =>
      line.toSeq.zipWithIndex.map { case (char, col) =>
        Point(row, col) -> char
      }
    }.collect {
      case (point, '*') => adjacency(point, width, height).flatMap { p =>
        partMap.get(p).toSeq
      }.toSet
    }.filter(_.size == 2).map(_.map(_.num.toLong).product)

    println(gearRatios.sum)
  }
}
