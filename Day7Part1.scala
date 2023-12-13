object Day7Part1 {
  object Card {
    val values = Seq(
      14 -> "A",
      13 -> "K",
      12 -> "Q",
      11 -> "J",
      10 -> "T"
    ) ++ (2 until 10).map(i => i -> i.toString())

    def valueFromString = values.map(i => i._2 -> i._1).toMap
    def valueToString = values.map(_ -> _).toMap

    def apply(value: String): Card = Card(valueFromString(value))
  }

  case class Card(value: Int) extends Ordered[Card] {
    override def toString(): String = Card.valueToString(value)
    def compare(that: Card): Int = this.value - that.value
  }

  object HandType:
    def counts(hand: Hand): Map[Card, Int] = {
      hand.cards.groupBy(card => card).mapValues(_.size).toMap
    }

    def countsByRank(hand: Hand): Seq[(Int, Card)] = {
      val out = counts(hand).toSeq.map((c, i) => (i, c)).sorted.reverse
      println(out.mkString("; "))
      out
    }

    def classify(hand: Hand): HandType = {
      val countSets = counts(hand).values.toSeq.sorted.reverse

      countSets match {
        case 5 +: _ => HandType.Five
        case 4 +: _ => HandType.Four
        case 3 +: 2 +: _ => HandType.Full
        case 3 +: _ => HandType.Three
        case 2 +: 2 +: _ => HandType.TwoPair
        case 2 +: _ => HandType.OnePair
        case _ => HandType.HighCard
      }
    }

  enum HandType extends Ordered[HandType]:
    case HighCard, OnePair, TwoPair, Three, Full, Four, Five

    def compare(that: HandType): Int = this.ordinal - that.ordinal

  case class Hand(cards: Seq[Card]) extends Ordered[Hand] {
    def handType: HandType = HandType.classify(this)

    def compare(that: Hand): Int =
      if (this.handType == that.handType)
        then this.cards compare that.cards
        else this.handType compare that.handType
  }

  case class Play(hand: Hand, bid: Long)

  def solve(inFile: String): Long = {
    val input = io.Source.fromFile(inFile).getLines().toSeq

    val PlayMatch = raw"^(.....)\s+(\d+)$$".r

    val plays = input.map {
      case PlayMatch(handInput, bidInput) => Play(Hand(handInput.map(c => Card(c.toString))), bidInput.toLong)
      case line => sys.error(line)
    }

    val ranked = plays.sortBy(_.hand).zipWithIndex.map((p, i) => (i + 1) -> p)

    println(ranked.mkString("\n"))

    ranked.map((r, p) => r * p.bid).sum
  }

  def main(args: Array[String]): Unit = {
    val winningsSmall = solve("day-7-input-small.txt")
    assert(winningsSmall == 6440)
    println(solve("day-7-input.txt"))
  }
}
