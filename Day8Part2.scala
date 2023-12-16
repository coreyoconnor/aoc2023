import scala.annotation.tailrec
object Day8Part2 {
  type Label = String
  type LabelIndex = Int
  type Insts = List[Char]

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

  case class Cycle(offset: Long, length: Long)

  type State = (Long, LabelIndex, Insts)

  def gcd(u: Long, v: Long): Long = {

    @tailrec def go(a: Long, b: Long): Long =
      if (b == 0) then a else {
        go(b, a % b)
      }

    if (u < v) then go(v, u) else go(u, v)
  }

  def lcm(vs: Seq[Long]): Long =
    vs.foldLeft(1L) { case (out, v) =>
      out * v / gcd(out, v)
    }

  def solve(filePath: String): Long = {
    val lines = scala.io.Source.fromFile(filePath).getLines().toSeq
    val insts = lines.head.toList
    val net = parseNet(lines.drop(2))
    val startIndices = net.labels.toSeq.filter(_._1.endsWith("A")).map(_._2)
    val targetIndices = net.labels.toSeq.filter(_._1.endsWith("Z")).map(_._2).toSet

    def findFirst(state: State)(p: LabelIndex => Boolean): Option[State] = {
      def go(current: State, history: Set[(LabelIndex, Insts)]): Option[State] =
        if (history.contains((current._2, current._3))) None else {
          if (current._3.isEmpty) go((current._1, current._2, insts), history) else {
            val next = current._3.head match {
              case 'L' => (current._1 + 1, net.assocs(current._2)._1, current._3.tail)
              case 'R' => (current._1 + 1, net.assocs(current._2)._2, current._3.tail)
            }

            if (p(current._2)) Some(next) else go(next, history + (current._2 -> current._3))
          }
        }

      go(state, Set.empty)
    }

    def allCycles(startIndex: LabelIndex): Seq[Cycle] = {
      val initialState: State = (0L, startIndex, insts)
      val out = targetIndices.toSeq.foldLeft(Seq.empty[Cycle]) { case (cycles, target) =>
        findFirst(initialState)(_ == target) match {
          case None => cycles
          case Some(startState) => {
            val cycleStart = startState._1 - 1
            println(s"cycle start $cycleStart")

            val repeatState = findFirst(startState)(_ == target).get
            val cycleRepeat = repeatState._1 - 1

            println(s"repeat at $cycleRepeat")
            val cycle = Cycle(cycleStart, cycleRepeat - cycleStart)
            cycles :+ cycle
          }
        }
      }

      out
    }

    val everyCycle = startIndices.flatMap(allCycles)
    println(everyCycle.mkString("\n"))
    assert(everyCycle.forall(c => c._1 == c._2))

    val out: Long = lcm(everyCycle.map(_._1))
    println(s"solve: $out")
    out
  }

  def main(args: Array[String]): Unit = {
    assert(solve("day-8-input-small-2.txt") == 6)

    solve("day-8-input.txt")
  }
}
