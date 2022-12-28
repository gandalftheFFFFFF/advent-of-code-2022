import scala.annotation.tailrec
import scala.io.BufferedSource

object Day6 {

  def main(args: Array[String]): Unit = {

    val input = scala.io.Source.fromFile("day-6-input.txt")


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
        println(s"Current: $currentChar ($index), last4: '$lastN' -> '$newLastN'")

        // Check if last 4 are unique
        val isUnique: Boolean = newLastN.trim.distinct.length == blockSize // Now is not the time for premature optimization

        // Return index if unique
        if (isUnique && newLastN.length == blockSize){
          println(s"Target index: $newIndex, newLastN: '$newLastN'")
          newIndex
        } else {
          // Else increment index and continue
          parse(chars, newLastN, newIndex, blockSize)
        }
      } else {
        0
      }
    }
    val blockSize = 4
    val initialInput = input.take(blockSize - 1).fold("")(_.toString + _.toString).toString
    parse(input, " " + initialInput, index = blockSize - 1, blockSize = blockSize)

  }
}
