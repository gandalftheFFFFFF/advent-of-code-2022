object Day3 {

  case class Item(kind: Char, priority: Int) // Kind, because `type` is reserved
  case object Item {
    def apply(kind: Char): Item = {
      val priority = if (kind <= 'Z') kind - 39 + 1 else kind - 97 + 1
      Item(kind, priority)
    }
  }

  case class Backpack(c1: Seq[Item], c2: Seq[Item]) {
    def commonItems: Set[Item] = {
      val firstSet: Set[Item] = c1.toSet
      val secondSet: Set[Item] = c2.toSet
      val intersect: Set[Item] = firstSet.intersect(secondSet)
      intersect
    }
  }
  case object Backpack {
    def apply(line: String): Backpack = {
      val items = line.toList.map(Item.apply)
      val split = items.splitAt(items.length/2)
      Backpack(split._1, split._2)
    }
  }

  def main(args: Array[String]): Unit = {

    val input: List[String] = scala.io.Source.fromFile("day-3-sample-input.txt").getLines().toList

    val backpacks = input.map(Backpack.apply)
    val commonItems = backpacks.flatMap(_.commonItems)
    println(commonItems)
    val part1: Int = input.map(Backpack.apply).flatMap(_.commonItems).map(_.priority).sum
    println(part1)

  }
}
