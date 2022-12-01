import scala.io.Source

object Main {

  case class Elf(calories: Int)

  def main(args: Array[String]): Unit = {
    val input: List[String] = Source.fromFile("input2.txt").getLines.toList

    def loop(lines: List[String]): List[Elf] = {
      def helper(remainder: List[String], currentElf: Elf, elves: List[Elf]): List[Elf] = remainder match {
        case Nil => currentElf :: elves  // Add the last elf to the list
        case h::t => {
          if (h == "") helper(remainder = t, currentElf = Elf(calories = 0), elves = currentElf :: elves)
          else {
            val calories: Int = h.toInt // Unsafe
            val sameElfMoreCals: Elf = currentElf.copy(calories = currentElf.calories + calories)
            helper(remainder = t, currentElf = sameElfMoreCals, elves = elves)
          }
        }
      }
      helper(lines, currentElf = Elf(calories = 0), elves = List.empty[Elf])
    }
    val elves: List[Elf] = loop(lines = input)

    def maxElf(current: Elf, remainder: List[Elf], record: Elf): Elf = {
      if (remainder.isEmpty) record
      else {
        if (current.calories > record.calories) maxElf(current = remainder.head, remainder = remainder.tail, record = current)
        else maxElf(current = remainder.head, remainder = remainder.tail, record = record)
      }
    }

    val biggestElf: Elf = maxElf(current = elves.head, remainder = elves.tail, record = elves.head)
    println(biggestElf)

    val top3Elves: Int = elves.sortBy(_.calories).takeRight(3).map(_.calories).sum
    println(top3Elves)




  }
}