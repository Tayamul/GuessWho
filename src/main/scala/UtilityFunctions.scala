import GamePlay.{characterToGuess, remainingCharacters}

object UtilityFunctions {

  // Finding a random character for you to guess
  def randomChars(individualsMap: Map[Int, Character], numberOfCharacters: Int = 24): Character = {
    val randomCharacter = new scala.util.Random
    val characterToGuess = individualsMap(randomCharacter.nextInt(numberOfCharacters) + 1)
    characterToGuess
  }

  // pattern matching characteristics you choose against the characters in the map
  def matchCharacteristics(individualsMap: Map[Int, Character], feature: String, value: Any): Map[Int, Character] = {
    individualsMap.filter {
      case (_, Male(name, hasHair, glasses, facialHair, hairColour, hat, eyeColour)) => feature match {
        case "name" => name == value
        case "hashair" => hasHair == value
        case "glasses" => glasses == value
        case "facialhair" => facialHair == value
        case "haircolour" => hairColour == value
        case "hat" => hat == value
        case "eyecolour" => eyeColour == value
        case "gender" => "male" == value
        case _ => true
      }
      case (_, Female(name, glasses, hairColour, hat, eyeColour)) => feature match {
        case "name" => name == value
        case "hashair" => true == value
        case "glasses" => glasses == value
        case "facialhair" => false == value
        case "haircolour" => hairColour == value
        case "hat" => hat == value
        case "eyecolour" => eyeColour == value
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
  def gameQuestions(remainingQuestions: Set[String], acceptedStrings : Seq[String]): Unit = {
    println("Ask another question.")
    println("You can pick from the following questions by typing the number:")
    acceptedStrings.filter {
      case feature => remainingQuestions.contains(feature)
    }.foreach{
      case "gender" => println("1. Are they _(male/female)_ ?")
      case "glasses" => println("2. Do they wear glasses?")
      case "facialhair" => println("3. Do they have facial hair?")
      case "hat" => println("4. Are they wearing a hat?")
      case "eyecolour" => println("5. Do they have _(colour)_ eyes?")
      case "hashair" => println("6. Do they have hair?")
      case "haircolour" => println("7. Is their hair _(colour)_ ?")
    }
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

  def intro (): Unit = {

  val BLACK = "\u001B[30m"
  val BOLD = "\u001B[1m"
  val UNDERLINE = "\u001B[4m"
  val RESET = "\u001B[0m"
  val BG_BRIGHT_GREEN = "\u001B[102m"
  val BG_BRIGHT_YELLOW = "\u001B[103m"

    println(s"${BG_BRIGHT_YELLOW}${BLACK}${BOLD}${UNDERLINE}!! * Ultimate Guess Who Experience * !!${RESET}")
    println()
    println(s"${BG_BRIGHT_GREEN}${BLACK}${BOLD}The rules are simple, in this one player game the computer selects a secret undercover agent you have to find. \n You will be given a list of helpful questions to ask. You can select your question using the question numbering. \n \n Example: \n 1. This is a question? \n Input: \n 1 \n \n when a question has multiple options shown by _(option) _ a follow up question will be asked if you choose this route. \n At anytime you can enter G to take a guess at who the spy is. \n But don't get it wrong or the game ends. \n You will be prompted to guess as you narrow down the search. \n Or, keep questioning until your down to the single man or woman! \n${RESET}")
    println()
  }

  def initialHelpRules (): Unit = {

    val BLACK = "\u001B[30m"
    val BOLD = "\u001B[1m"
    val RESET = "\u001B[0m"
    val BG_BRIGHT_GREEN = "\u001B[102m"


    println(s"${BG_BRIGHT_GREEN}${BLACK}${BOLD}Whenever you are prompted for an answer you can also access the following commands:" +
      s"\n Enter B -> To see the remaining characters on the board and their attributes \n Enter G -> To make a guess at who the character is \n Enter E -> To leave the game :( \n Enter R -> To see the rules and how to win  \n Enter H -> At any time to see these options again ${RESET}")
    println()
  }

  def goodLuck (): Unit = {

    val BG_BRIGHT_WHITE = "\u001B[107m"
    val RED = "\u001B[31m"
    val GREEN = "\u001B[32m"
    val YELLOW = "\u001B[33m"
    val BLUE = "\u001B[34m"
    val PURPLE = "\u001B[35m"
    val CYAN = "\u001B[36m"
    val WHITE = "\u001B[37m"
    val BOLD = "\u001B[1m"
    val UNDERLINE = "\u001B[4m"
    val RESET = "\u001B[0m"

    println(s"${BG_BRIGHT_WHITE}${BOLD}${UNDERLINE}${BLUE}G${RED}O${GREEN}O${YELLOW}D ${BLUE}L${PURPLE}U${CYAN}C${WHITE}K${RESET}")
    println()
  }

  def hairColourMatch(response: String, remainingHairColours: Seq[String]): String = {
    println("\nSelect the hair colour:")
    remainingHairColours.foreach {
      case (element) => print(f"$element " + "\n")
    }
    println()
    var value = scala.io.StdIn.readLine().toLowerCase()
    println()
    value match {
      case "1" => {
        println("Is their hair brown?")
        value = "brown"
      }
      case "2" => {
        println("Is their hair blonde?")
        value = "blonde"
      }
      case "3" => {
        println("Is their hair black?")
        value = "black"
      }
      case _ => {
        println("That is not an accepted input please try again")
        hairColourMatch(response, remainingHairColours)
      }
    }
    println()
    value
  }

  def eyeColourMatch(response: String, remainingEyeColours: Seq[String]): String = {

    println("\nWhich colour eye:")
    remainingEyeColours.foreach {
      case (element) => print(f"$element" + "\n")

    }

    println()
    var value = scala.io.StdIn.readLine().toLowerCase()
    println()
    value match {
      case "1" => {
        println("Do they have brown eyes?")
        value = "brown"
      }
      case "2" => {
        println("Do they have blue eyes?")
        value = "blue"
      }
      case "3" => {
        println("Do they have green eyes?")
        value = "green"
      }
      case _ =>
        println("That is not an accepted input please try again")
        eyeColourMatch(response,remainingEyeColours)
    }
    println()
    value
  }

  def genderMatch (): String = {

      println("\nSelect the gender:")
      println("1. male")
      println("2. female")
      println()
      var value = scala.io.StdIn.readLine().toLowerCase()
      println()
      if(value == "1." || value == "1") {
        println("1. Are they male?")
        value = "male"
      } else if (value == "2." || value == "2") {
        println("1. Are they female?")
        value = "female"
      } else {
        println("That is not an accepted input sorry ")
        genderMatch()
      }
      println()
      value
    }

  def formatValForRemove (value: Any) : String = {
    value match {
      case "brown" => "1. brown"
      case "blonde" => "2. blonde"
      case "black" => "3. black"
      case "blue" => "2. blue"
      case "green" => "3. green"
      case _ => ""
    }
  }

}
