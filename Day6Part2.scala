object Day6Part2 {
  /* (total - pressed) * pressed = distance
   * EG:
   * total = 7
   * pressed = 3
   * distance = 4 * 3 = 12
   *
   * total * pressed - pressed^2 = distance
   *
   * We want for distance > max_distance
   *
   * total * pressed - pressed^2 - max_distance > 0
   * -pressed^2 + total * pressed - max_distance > 0
   *  -1*pressed^2 + total * pressed - max_distance > 0
   *  -1*4 + 7 * 2 - 9 = 14 - 9 - 4 = 5 - 4 = 1
   *  -1*25 + 7 * 5 - 9 = 35 - 25 - 9 = 1
   *
   * quadratic is:
   * x = [-b ± sqrt(b^2 - 4ac)] / 2a.
   * x = [-total ± sqrt(total^2 - 4*-1*-max_d)] / 2*-1
   * x = -[-total ± sqrt(total^2 - 4*max_d)] / 2
   * x = total ± sqrt(total^2 - 4*max_d) / 2
   *
   * x_0 = (total + sqrt(total^2 - 4*max_d)) / 2
   * x_1 = (total - sqrt(total^2 - 4*max_d)) / 2
   * x_0 = (7 + sqrt(7^2 - 4*9)) / 2
   * x_1 = (7 - sqrt(7^2 - 4*9)) / 2
   * x_0 = (7 + sqrt(49 - 36)) / 2
   * x_1 = (7 - sqrt(49 - 36)) / 2
   * x_0 = (7 + 3.6) / 2
   * x_1 = (7 - 3.6) / 2
   * x_0 = (10.6) / 2 = 5.2
   * x_1 = (3.4) / 2 = 1.7
   * floor(x_0) = 5
   * ceil(x_1) = 2
   */

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("day-6-input-part-2.txt").getLines().toSeq.map(_.split("\\s+"))
    val times = input(0).tail.map(_.toLong)
    val distances = input(1).tail.map(_.toLong)

    val races = times.zip(distances)

    println(races.mkString("; "))

    val winningWays = races.map { case (total, record) =>
      val justOver = record + 0.01
      val high0 = (total.toDouble + Math.sqrt(total.toDouble*total.toDouble - 4.0 * justOver.toDouble)) / 2.0
      val low0 = (total.toDouble - Math.sqrt(total.toDouble*total.toDouble - 4.0 * justOver.toDouble)) / 2.0
      val highPressed = Math.floor(high0).toLong min total
      val lowPressed = Math.ceil(low0).toLong max 0

      (highPressed - lowPressed + 1)
    }

    println(winningWays.mkString("; "))

    println(winningWays.fold(1L)(_ * _))
  }
}
