object Day2Part1 {
  enum Colors:
    case Red, Green, Blue

  case class Draw(red: Int, green: Int, blue: Int)

  case class Game(id: Int, draws: Seq[Draw])

  val maxRed = 12
  val maxGreen = 13
  val maxBlue = 14

  def validGame(game: Game): Boolean =
    game.draws.forall { draw =>
      draw.red <= maxRed && draw.green <= maxGreen && draw.blue <= maxBlue
    }

  def parseGame(line: String): Game = {
    val MatchGame = raw"^Game (\d+):(.*)$$".r

    line match {
      case MatchGame(id, drawInput) => Game(id.toInt, parseDraws(drawInput))
      case _ => sys.error(s"invalid line $line")
    }
  }

  def parseDraws(input: String): Seq[Draw] = {
    val MatchDraw = raw" (\d+) (red|green|blue)".r
    input.split(";") map { drawInput =>
      val (reds, greens, blues) = drawInput.split(",").foldLeft((0, 0, 0)) { case ((r,g,b), draw) =>
        draw match {
          case MatchDraw(count, color) => color match {
            case "red" => (r + count.toInt, g, b)
            case "green" => (r, g + count.toInt, b)
            case "blue" => (r, g, b + count.toInt)
            case _ => sys.error(s"invalid color $color")
          }
          case _ => sys.error(s"invalid draw $draw")
        }
      }
      Draw(reds, greens, blues)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("day-2-input.txt", "UTF-8").getLines.toSeq

    val games = input.map(parseGame)
    val validGames = games.filter(validGame)
    val validGameIds = validGames.map(_.id)

    println(validGameIds.sum)
  }
}

