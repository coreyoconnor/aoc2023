import scala.collection.mutable

object Day3Part1 {
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

  class AdjCoverage(val width: Int, val height: Int) {
    val coverage = mutable.Buffer.fill(width * height)(false)

    def set(row: Int, col: Int): Unit =
      if (row >= 0 && row < height && col >= 0 && col < width) coverage.update(row*width+col, true)

    def add(row: Int, col: Int): Unit = {
      for {
        dx <- Seq(-1, 0, 1)
        dy <- Seq(-1, 0, 1)
      } set(row + dy, col + dx)
    }

    def covers(point: Point): Boolean = coverage(point.row * width + point.col)

    override def toString(): String = coverage.map {
      case true => 'X'
      case false => ' '
    }.grouped(width).mkString("\n")
  }

  def buildAdjCoverage(input: Seq[String]): AdjCoverage = {
    val width = input.head.size
    val height = input.size
    val out = new AdjCoverage(width, height)

    input.zipWithIndex foreach { case (line, row) =>
      line.toSeq.zipWithIndex foreach { case (char, col) =>
        if (!char.isDigit && char != '.') {
          out.add(row, col)
        }
      }
    }

    out
  }

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("day-3-input.txt", "UTF-8").getLines().toSeq

    val parts: Seq[Part] = parseParts(input)
    println(parts.mkString("\n"))

    val adjCoverage: AdjCoverage = buildAdjCoverage(input)
    println(adjCoverage.toString())

    val validParts = parts filter { part =>
      val points = part.coverage
      points.exists(adjCoverage.covers)
    }

    println(validParts.mkString("\n"))
    println(validParts.map(_.num).sum)
    // sum of part numbers
  }
}
