object Day1Part2 {
  val digitWords = Seq(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  val toDigit = digitWords.toMap ++ digitWords.map(_._2).map(d => d.toString -> d)
  val digitMatch = toDigit.keys.mkString("|").r

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("day-1-input.txt", "UTF-8").getLines().filterNot(_.isBlank).toSeq
    def allMatches(remaining: String): Seq[String] =
      digitMatch.findFirstMatchIn(remaining) match {
        case None => Seq.empty
        case Some(value) => value.matched +: allMatches(remaining.drop(value.start + 1))
      }

    val values = input.map { line =>
      val matches = allMatches(line)
      toDigit(matches.head) * 10 + toDigit(matches.last)
    }
    println(values.mkString("\n"))

    println(values.sum)
  }
}

