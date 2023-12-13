object Day7Part2 {
  object Card {
    val values = Seq(
      14 -> "A",
      13 -> "K",
      12 -> "Q",
      1 -> "J",
      10 -> "T"
    ) ++ (2 until 10).map(i => i -> i.toString())

    def valueFromString = values.map(i => i._2 -> i._1).toMap
    def valueToString = values.map(_ -> _).toMap
    val all = values.map((value, _) => Card(value))

    val joker = Card("J")
    val notJoker: Seq[Card] = Card.all.filter(_ != joker)

    def apply(value: String): Card = Card(valueFromString(value))
  }

  case class Card(value: Int) extends Ordered[Card] {
    override def toString(): String = Card.valueToString(value)
    def compare(that: Card): Int = this.value - that.value
  }

  type Cards = Seq[Card]

  object HandType:
    def counts(cards: Cards): Map[Card, Int] = {
      cards.groupBy(card => card).mapValues(_.size).toMap
    }

    def classify(cards: Cards): HandType = {
      val countSets = counts(cards).values.toSeq.sorted.reverse

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

  case class Hand(cards: Cards, bestCards: Cards) extends Ordered[Hand] {
    def handType: HandType = HandType.classify(this.bestCards)

    def compare(that: Hand): Int =
      if (this.handType == that.handType)
        then this.cards compare that.cards
        else this.handType compare that.handType
  }

  case class Play(hand: Hand, bid: Long)

  def findBestCards(cards: Cards): Cards = {
    def pick(card: Card): Cards =
      if (card == Card.joker) Card.notJoker else Seq(card)

    def pickAll(remaining: Cards, out: Seq[Cards]): Seq[Cards] = {
      if (remaining.isEmpty) then out else {
        val allPicks = pick(remaining.head) flatMap { card =>
          out.map(card +: _)
        }
        pickAll(remaining.tail, allPicks)
      }
    }

    given Ordering[Cards] with
      override def compare(x: Cards, y: Cards): Int =
        HandType.classify(x) compare HandType.classify(y)

    pickAll(cards.tail, pick(cards.head).map(Seq(_))).sorted(Ordering[Cards].reverse).head
  }

  def solve(inFile: String): Long = {
    val input = io.Source.fromFile(inFile).getLines().toSeq

    val PlayMatch = raw"^(.....)\s+(\d+)$$".r

    val plays = input.map {
      case PlayMatch(handInput, bidInput) => {
        val cards = handInput.map(c => Card(c.toString))
        val bestCards = findBestCards(cards)
        val hand = Hand(cards, bestCards)
        Play(hand, bidInput.toLong)
      }
      case line => sys.error(line)
    }

    val ranked = plays.sortBy(_.hand).zipWithIndex.map((p, i) => (i + 1) -> p)

    println(ranked.mkString("\n"))

    ranked.map((r, p) => r * p.bid).sum
  }

  def main(args: Array[String]): Unit = {
    val winningsSmall = solve("day-7-input-small.txt")
    println(winningsSmall)
    assert(winningsSmall == 5905)
    println(solve("day-7-input.txt"))
  }
}
