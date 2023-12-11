object Day5Part2 {
  type SeedId = Long
  type LocationId = Long
  type Count = Long

  type SeedRange = (SeedId, Count)

  def parseSeedRanges(input: String): Seq[SeedRange] = {
    val MatchSeeds = raw"seeds: (.+)+$$".r

    MatchSeeds.findFirstMatchIn(input).get.group(0).split("\\s+").tail.grouped(2).toSeq.map { r =>
      (r(0).toLong, r(1).toLong)
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

  case class RangeMap(ranges: Seq[(Long, Long, Long)]) extends Function[SeedRange, Seq[SeedRange]] {
    def apply(v: SeedRange): Seq[SeedRange] = {
      ranges.collectFirst {
        // 1, 1, 3
        // [2, 3)
        // starts in range
        case (dest, src, count) if v._1 >= src && v._1 < src + count => {
          // 1
          val i = v._1 - src
          if (v._1 + v._2 <= src + count) {
            Seq((dest + i, v._2))
          } else {
            // (1 + 1 = 2, 2) +: apply((4, 1))
            (dest + i, count - i) +: apply((src + count, v._2 - (count - i)))
          }
        }

        // stops in range but starts before
        case (dest, src, count) if v._1 + v._2 > src && v._1 + v._2 <= src + count => {
          (dest, v._1 + v._2 - src) +: apply((v._1, src - v._1))
        }

        // starts before and stops after
        case (dest, src, count) if v._1 < src && v._1 + v._2 > src + count => {
          (dest, count) +: (apply((v._1, src - v._1)) ++ apply((src + count, v._1 + v._2 - src - count)))
        }
      } getOrElse Seq(v)
    }
  }

  def parseMap(block: Block): RangeMap = {
    // dest, src, count
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

    RangeMap(ranges)
  }

  def lowestLocation(input: Seq[String]): LocationId = {
    val blocks = parseBlocks(input)

    println(blocks.mkString("\n"))

    val seedRanges = parseSeedRanges(blocks(0).head)
    println(seedRanges)

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

    val locRanges = seedRanges flatMap seedsToSoil flatMap soilToFert flatMap fertToWater flatMap waterToLight flatMap lightToTemp flatMap tempToHumid flatMap humidToLoc

    println(locRanges.mkString("\n"))

    locRanges.sortBy(_._1).head._1
  }

  def main(args: Array[String]): Unit = {
    val testInput = scala.io.Source.fromFile("day-5-input-small.txt").getLines().toSeq

    assert(lowestLocation(testInput) == 46)

    val input = scala.io.Source.fromFile("day-5-input.txt").getLines().toSeq

    println(lowestLocation(input))
  }
}
