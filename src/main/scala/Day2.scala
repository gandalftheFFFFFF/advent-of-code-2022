object Day2 {

  // Represents the outcome of a rock, paper, scissors round
  sealed trait Outcome {
    val points: Int
    def playerSymbolForOutcome(opponent: Symbol): Symbol = {
      // The symbol (rock, paper or scissors) required for this outcome given opponent hand/symbol
      this match {
        case Win => opponent.defeatedBy
        case Loss => opponent.winsOver
        case Draw => opponent
      }
    }
  }
  case object Win extends Outcome {val points: Int = 6}
  case object Loss extends Outcome {val points: Int = 0}
  case object Draw extends Outcome {val points: Int = 3}

  // Represents a choice of rock, paper or scissors
  sealed trait Symbol {
    val points: Int
    def versus(other: Symbol): Outcome
    def defeatedBy: Symbol
    def winsOver: Symbol
  }
  case object Rock extends Symbol {
    val points: Int = 1
    override def defeatedBy: Symbol = Paper
    override def winsOver: Symbol = Scissors
    override def versus(other: Symbol): Outcome = other match {
      case Rock => Draw
      case Paper => Loss
      case Scissors => Win
    }
  }
  case object Paper extends Symbol {
    val points: Int = 2
    override def defeatedBy: Symbol = Scissors
    override def winsOver: Symbol = Rock
    override def versus(other: Symbol): Outcome = other match {
      case Rock => Win
      case Paper => Draw
      case Scissors => Loss
    }
  }
  case object Scissors extends Symbol {
    val points: Int = 3
    override def defeatedBy: Symbol = Rock
    override def winsOver: Symbol = Paper
    override def versus(other: Symbol): Outcome = other match {
      case Rock => Loss
      case Paper => Win
      case Scissors => Draw
    }
  }

  // Round used for part 1 of the problem
  case class Round(opponent: Symbol, player: Symbol) {
    def roundPoints: Int = {
      val symbolValue = player.points
      val outcomeValue = player.versus(opponent).points
      symbolValue + outcomeValue
    }
  }

  case object Round {
    def apply(line: String): Round = {
      val symbols = line.split(" ").map({
        case "A" | "X" => Rock
        case "B" | "Y" => Paper
        case "C" | "Z" => Scissors
      })
      Round(symbols(0), symbols(1))  // Unsafe but let's trust the input :)
    }
  }

  // Round used for part 2 of the problem
  case class Round2(opponent: Symbol, outcome: Outcome) {
    def roundPoints: Int = {
      val playerSymbolValue = outcome.playerSymbolForOutcome(opponent).points
      val outcomeValue = outcome.points
      playerSymbolValue + outcomeValue
    }
  }

  case object Round2 {
    def apply(line: String): Round2 = {
      val opponent: Symbol = line.substring(0, 1) match {
        case "A" => Rock
        case "B" => Paper
        case "C" => Scissors
      }
      val outcome: Outcome = line.substring(2,3) match {
        case "X" => Loss
        case "Y" => Draw
        case "Z" => Win
      }
      Round2(opponent, outcome)
    }
  }



  def main(args: Array[String]): Unit = {

    val input: List[String] = scala.io.Source.fromFile("day-2-input.txt").getLines().toList

    // Part 1:
    val rounds = input.map(Round.apply)  // Same as input.map(line => Round(line))
    val part1: Int = rounds.map(_.roundPoints).sum
    println(part1)

    // Part 2:
    val round2s = input.map(Round2.apply)
    val part2 = round2s.map(_.roundPoints).sum
    println(part2)


  }
}
