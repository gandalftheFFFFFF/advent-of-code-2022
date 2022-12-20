object Day4 {

  case class Section(range: Range) {
    def contains(otherSection: Section): Boolean = {
      this.range.containsSlice(otherSection.range)  // Order matters for containsSlice - maybe non-issue?
    }
  }
  case object Section {
    def apply(rangeString: String): Section = {
      val split = rangeString.split("-").map(_.toInt)
      val start = split(0) // Meh
      val end = split(1) // Meh
      Section(Range.inclusive(start, end))
    }
  }
  case class Pair(section1: Section, section2: Section) {
    def anyContained: Boolean = {
      val first = section1.contains(section2)
      val second = section2.contains(section1)
      first || second
    }

    def isOverlapping: Boolean = {
      section1.range.intersect(section2.range).sum > 0
    }
  }
  case object Pair {
    def apply(pairString: String): Pair = {
      val split = pairString.split(",")
      val pair1 = split(0)
      val pair2 = split(1)
      Pair(Section(pair1), Section(pair2))
    }
  }

  def main(args: Array[String]): Unit = {

    val input: List[String] = scala.io.Source.fromFile("day-4-input.txt").getLines().toList
    val mapped = input.map(Pair.apply)

    val part1 = mapped.count(_.anyContained)
    println(part1)
    // Part 1

    // Part 2
    val part2 = mapped.count(_.isOverlapping)
    print(part2)
  }
}
