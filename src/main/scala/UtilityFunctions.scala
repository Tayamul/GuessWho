object UtilityFunctions {

  // Finding a random character for you to guess
  def randomChars(individualsMap: Map[Int, Character]): Character = {
    val randomCharacter = new scala.util.Random
    val characterToGuess = individualsMap(randomCharacter.nextInt(24) + 1)
    characterToGuess
  }

  // pattern matching characteristics you choose against the characters in the map
  def matchCharacteristics(individualsMap: Map[Int, Character], feature: String, value: Any): Map[Int, Character] = {
    individualsMap.filter {
      case (_, BaldMan(name, glasses, facialHair, hat, eyeColour)) => feature match {
        case "name" => name == value
        case "glasses" => glasses == value
        case "facialhair" => facialHair == value
        case "hat" => hat == value
        case "eyecolour" => eyeColour == value
        case "hair" => false == value
        case "haircolour" => true == value
        case "gender" => "male" == value
        case _ => true
      }
      case (_, HairMan(name, glasses, facialHair, hairColour, hat, eyeColour)) => feature match {
        case "name" => name == value
        case "glasses" => glasses == value
        case "facialhair" => facialHair == value
        case "haircolour" => hairColour == value
        case "hat" => hat == value
        case "eyecolour" => eyeColour == value
        case "hair" => true == value
        case "gender" => "male" == value
        case _ => true
      }
      case (_, Female(name, glasses, hairColour, hat, eyeColour)) => feature match {
        case "name" => name == value
        case "glasses" => glasses == value
        case "haircolour" => hairColour == value
        case "facialhair" => false == value
        case "hat" => hat == value
        case "eyecolour" => eyeColour == value
        case "hair" => true == value
        case "gender" => "female" == value
        case _ => true
      }
      case _ => false
    }
  }

  // Play round function returns the filtered characters from pattern matching
  def playRound(characters: Map[Int, Character], feature: String, value: Any): Map[Int, Character] = {
    val remainingCharacters = matchCharacteristics(characters: Map
      [Int, Character], feature, value)
    remainingCharacters
  }

  // Tells you what questions are still to be asked
  def gameQuestions(remainingQuestions: Set[String]): Unit = {
    println("Ask another question.")
    println("You can pick from the following categories:")
    remainingQuestions.foreach(feature => {
      println(feature)
    })
  }


  def printAllRemainingChars(remainingCharacters: Map [Int, Character]): Unit = {
  val RESET = "\u001B[0m"
  val BG_WHITE = "\u001B[47m"
  val BLACK = "\u001B[30m"

    println("Remaining Characters on the board:")
    remainingCharacters.foreach {
      case (_, value) => {
        val name = s"${BG_WHITE}${BLACK} Name: ${RESET} ${value.name}"
        val glasses = s"${BG_WHITE}${BLACK} Glasses: ${RESET} ${value.glasses}"
        val hat = s"${BG_WHITE}${BLACK} Hat: ${RESET} ${value.hat}"
        val hair = s"${BG_WHITE}${BLACK} Hair: ${RESET} ${value.hasHair}"
        val hairColour = if (value.hasHair) s"${BG_WHITE}${BLACK} Hair Colour: ${RESET} ${value.hairColour}" else s"${BG_WHITE}${BLACK} Hair Colour: ${RESET} Bald"
        val facialHair = s"${BG_WHITE}${BLACK} Facial Hair: ${RESET} ${value.facialHair}"
        val eyeColour = s"${BG_WHITE}${BLACK} Eye Colour: ${RESET} ${value.eyeColour}"
        val gender = s"${BG_WHITE}${BLACK} Male or Female: ${RESET} ${value.gender}"

        println(f"$name%-35s $glasses%-35s $hat%-30s $hair%-30s $hairColour%-35s $facialHair%-35s $eyeColour%-35s $gender%-35s")

      }
    }
  }

  def helpMe(): Unit = {
    val BG_BRIGHT_GREEN = "\u001B[102m"
    val BOLD = "\u001B[1m"
    val BLACK = "\u001B[30m"
    val RESET = "\u001B[0m"

    println(s"${BG_BRIGHT_GREEN}${BLACK}${BOLD} Welcome to the Help Menu: \n Enter B -> To see the remaining characters on the board and their attributes \n Enter G -> To make a guess at who the character is \n Enter E -> To leave the game :( \n Enter R -> To see the rules and how to win  \n Enter H -> At any time for help ... As you know...  ${RESET}")
  }

}
