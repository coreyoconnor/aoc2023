object Day4Part2 {
  case class Card(num: Int, winning: Seq[Int], mine: Seq[Int])

  val MatchCard = raw"^Card +(\d+): +(.+) \| +(.+)$$".r

  def parseCards(input: Seq[String]): Seq[Card] =
    input map {
      case MatchCard(numInput, winningInput, mineInput) => {
        Card(numInput.toInt, winningInput.split(" +").map(_.toInt), mineInput.split(" +").map(_.toInt))
      }
      case line => sys.error(s"invalid input $line")
    }

  type Counts = Map[Int, Int]

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("day-4-input.txt", "UTF-8").getLines.toSeq

    val cards = parseCards(input)

    val matchedCards = cards.map { card =>
      card.winning.toSet.intersect(card.mine.toSet).size
    }

    println(matchedCards.mkString("\n"))

    val initialCounts = (0 until cards.size).map(_ -> 1).toMap

    val totalCounts = matchedCards.zipWithIndex.foldLeft(initialCounts) { case (counts, (matched, cardIndex)) =>
      val multiples = counts(cardIndex)
      ((cardIndex + 1) until (cardIndex + matched + 1)).toSeq.foldLeft(counts) { case (c, i) =>
        c.updatedWith(i) {
          case Some(count) => Some(count + multiples)
        }
      }
    }

    val totalCards = totalCounts.values.sum
    println(totalCards)
  }
}

