object Day4Part1 {
  case class Card(num: Int, winning: Seq[Int], mine: Seq[Int])

  val MatchCard = raw"^Card +(\d+): +(.+) \| +(.+)$$".r

  def parseCards(input: Seq[String]): Seq[Card] =
    input map {
      case MatchCard(numInput, winningInput, mineInput) => {
        Card(numInput.toInt, winningInput.split(" +").map(_.toInt), mineInput.split(" +").map(_.toInt))
      }
      case line => sys.error(s"invalid input $line")
    }

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("day-4-input.txt", "UTF-8").getLines.toSeq

    val cards = parseCards(input)

    println(cards.mkString("\n"))

    val scores = cards.map { card =>
      val matchedNumbers = card.winning.toSet.intersect(card.mine.toSet)
      if (matchedNumbers.size == 0) 0 else 1 << (matchedNumbers.size - 1)
    }

    println(scores.mkString("\n"))
    println(scores.sum)
  }
}

