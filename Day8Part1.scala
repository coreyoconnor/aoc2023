object Day8Part1 {
  type Label = String
  type LabelIndex = Int

  case class Net(
    assocs: IndexedSeq[(LabelIndex, LabelIndex)],
    labels: Map[Label, LabelIndex]
  )

  def parseNet(lines: Seq[String]): Net = {
    val MatchNode = raw"^(\w+)\s+=\s+\((\w+),\s+(\w+)\)$$".r

    val nodes = lines collect {
      case MatchNode(label, labelLeft, labelRight) => label -> (labelLeft, labelRight)
    }

    val labels = nodes.map(_._1).zipWithIndex.toMap

    val assocs = nodes.map { case (label, (labelLeft, labelRight)) =>
      (labels(labelLeft), labels(labelRight))
    }.toArray

    Net(assocs, labels)
  }

  def solve(filePath: String): Long = {
    val lines = scala.io.Source.fromFile(filePath).getLines().toSeq
    val insts = lines.head.toList
    val net = parseNet(lines.drop(2))
    val startIndex = net.labels("AAA")
    val targetIndex = net.labels("ZZZ")
    println(net)

    @annotation.tailrec
    def go(count: Long, current: LabelIndex, remaining: Seq[Char]): Long =
      if (current == targetIndex) count else {
        if (remaining.isEmpty) go(count, current, insts) else {
          remaining.head match {
            case 'L' => go(count + 1, net.assocs(current)._1, remaining.tail)
            case 'R' => go(count + 1, net.assocs(current)._2, remaining.tail)
          }
        }
      }

    val out = go(0, startIndex, insts)
    println(s"solve: $out")
    out
  }

  def main(args: Array[String]): Unit = {
    assert(solve("day-8-input-small-0.txt") == 2)
    assert(solve("day-8-input-small-1.txt") == 6)

    solve("day-8-input.txt")
  }
}
