object Day1Part1 {
  val FirstCalibrationValue = raw"^[^\d]*(\d).*$$".r

  val SecondCalibrationValue = raw"^.*(\d)[^\d]*$$".r

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("day-1-input.txt", "UTF-8").getLines().filterNot(_.isBlank).toSeq

    val firstDigits = input.map {
      case FirstCalibrationValue(a) => a
    }

    val secondDigits = input.map {
      case SecondCalibrationValue(b) => b
    }

    val calibrationValues = firstDigits.zip(secondDigits).map {
      case (a, b) => s"$a$b".toInt
    }
    println(calibrationValues.sum)
  }
}
