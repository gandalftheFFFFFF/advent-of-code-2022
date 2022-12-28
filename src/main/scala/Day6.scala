import scala.annotation.tailrec
import scala.io.BufferedSource

object Day6 {

  def main(args: Array[String]): Unit = {

    /*
    mjqjpqmgbljsphdztnvjfqwrcgsmlb
    bvwbjplbgvbhsrlpgdmjqwftvncz
    nppdvjthqldpwncqszvftbrmjlhg
    nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg
    zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw
     */

    @tailrec
    def parse(chars: BufferedSource, lastN: String, index: Int, blockSize: Int): Int = {
      if (chars.hasNext) {
        // Still chars to go
        val newIndex = index + 1
        val currentChar = chars.next()

        // Update last 4
        val newLastN: String = lastN.substring(1,blockSize) + currentChar.toString

        // Check if last 4 are unique
        val isUnique: Boolean = newLastN.trim.distinct.length == blockSize // Now is not the time for premature optimization

        // Return index if unique
        if (isUnique && newLastN.length == blockSize){
          newIndex
        } else {
          // Else increment index and continue
          parse(chars, newLastN, newIndex, blockSize)
        }
      } else {
        0
      }
    }
    val input1 = scala.io.Source.fromFile("day-6-input.txt")
    val blockSizePart1 = 4
    val initialInput = input1.take(blockSizePart1 - 1).fold("")(_.toString + _.toString).toString
    val part1 = parse(input1, " " + initialInput, index = blockSizePart1 - 1, blockSize = blockSizePart1)
    println(part1)

    val input2 = scala.io.Source.fromFile("day-6-input.txt")
    val blockSizePart2 = 14
    val initialInput2 = input2.take(blockSizePart2 - 1).fold("")(_.toString + _.toString).toString
    val part2 = parse(input2, " " + initialInput2, index = blockSizePart2 - 1, blockSize = blockSizePart2)
    println(part2)

  }
}
