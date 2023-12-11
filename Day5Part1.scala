object Day5Part1 {
  type SeedId = Long
  type LocationId = Long

  def parseSeeds(input: String): Seq[SeedId] = {
    val MatchSeeds = raw"seeds: (.+)$$".r
    input match {
      case MatchSeeds(seeds) => seeds.split("\\s+").map(_.toLong)
      case _ => sys.error(s"invalid seeds $input")
    }
  }

  type Block = Seq[String]

  def parseBlocks(input: Seq[String]): Seq[Block] = {
    def go(acc: Block, out: Seq[Block], remaining: Seq[String]): Seq[Block] =
      if (remaining.isEmpty) out :+ acc else {
        if (remaining.head.isEmpty()) {
          go(Seq.empty, out :+ acc, remaining.tail)
        } else {
          go(acc :+ remaining.head, out, remaining.tail)
        }
      }

    go(Seq.empty, Seq.empty, input)
  }

  case class RangMap(ranges: Seq[(Long, Long, Long)]) extends Function[Long, Long] {
    def apply(srcId: Long): Long = {
      ranges.collectFirst { case (dest, src, count) if srcId >= src && srcId < src + count =>
        srcId - src + dest
      } getOrElse srcId
    }
  }

  def parseMap(block: Block): Long => Long = {
    val RangeMatch = raw"^(\d+) (\d+) (\d+)$$".r

    val ranges = block.tail.map {
      case RangeMatch(destStr, srcStr, countStr) => {
        val dest = destStr.toLong
        val src = srcStr.toLong
        val count = countStr.toLong
        (dest, src, count)
      }
      case line => sys.error(s"invalid range $line")
    }

    RangMap(ranges)
  }

  def lowestLocation(input: Seq[String]): LocationId = {
    val blocks = parseBlocks(input)

    println(blocks.mkString("\n"))

    val seeds = parseSeeds(blocks(0).head)
    println(seeds)

    val seedsToSoil = parseMap(blocks(1))
    println("seedsToSoil")
    val soilToFert = parseMap(blocks(2))
    println("soilToFert")
    val fertToWater = parseMap(blocks(3))
    println("fertToWater")
    val waterToLight = parseMap(blocks(4))
    println("waterToLight")
    val lightToTemp = parseMap(blocks(5))
    println("lightToTemp")
    val tempToHumid = parseMap(blocks(6))
    println("tempToHumid")
    val humidToLoc = parseMap(blocks(7))
    println("humidToLoc")

    val seedsToLoc = Seq(
      soilToFert,
      fertToWater,
      waterToLight,
      lightToTemp,
      tempToHumid,
      humidToLoc
    ).foldLeft(seedsToSoil){ case (m0, m1) => m0 andThen m1 }

    println("built maps")

    val locations = seeds.map(seedsToLoc)

    println(locations)

    locations.min
  }

  def main(args: Array[String]): Unit = {
    val testInput = scala.io.Source.fromFile("day-5-input-small.txt").getLines().toSeq

    assert(lowestLocation(testInput) == 35)

    val input = scala.io.Source.fromFile("day-5-input.txt").getLines().toSeq

    println(lowestLocation(input))
  }
}
