object Day3 {

  case class Item(kind: Char, priority: Int) // Kind, because `type` is reserved
  case object Item {
    def apply(kind: Char): Item = {
      val priority = if (kind <= 'Z') kind - 39 + 1 else kind - 97 + 1
      Item(kind, priority)
    }
  }

  case class Rucksack(c1: Seq[Item], c2: Seq[Item]) {
    def commonItems: Set[Item] = {
      val firstSet: Set[Item] = c1.toSet
      val secondSet: Set[Item] = c2.toSet
      val intersect: Set[Item] = firstSet.intersect(secondSet)
      intersect
    }
  }
  case object Rucksack {
    def apply(line: String): Rucksack = {
      val items = line.toList.map(Item.apply)
      val split = items.splitAt(items.length/2)
      Rucksack(split._1, split._2)
    }
  }

  case class Group(val backpacks: Seq[Rucksack]) {
    def groupBadge: Item = {
      val allRucksackItems = backpacks.map(items => items.c1.toSet.union(items.c2.toSet))
      val initialItemSet = allRucksackItems.reduce(_ ++ _) // Create a starter set to intersect with for the fold
      val intersection = allRucksackItems.fold(initialItemSet)((s1, s2) => s1.intersect(s2))
      val groupBadge = intersection.head // Let's assume input only gives one badge per group
      groupBadge
    }
  }
  case object Group {
    def apply(lines: List[String]): Group = {
      val x = lines.map(Rucksack.apply)
      Group(x)
    }
  }


  def main(args: Array[String]): Unit = {

    val input: List[String] = scala.io.Source.fromFile("day-3-input.txt").getLines().toList

    // Part 1
    val backpacks = input.map(Rucksack.apply)
    val commonItems = backpacks.flatMap(_.commonItems)
    println(commonItems)
    val part1: Int = input.map(Rucksack.apply).flatMap(_.commonItems).map(_.priority).sum
    println(part1)

    // Part 2
    val groups = input.sliding(3, 3).toSeq.map(Group.apply)
    val groupBadges = groups.map(_.groupBadge)
    val part2 = groupBadges.map(_.priority).sum
    println(part2)
  }
}
