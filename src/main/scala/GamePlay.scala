import UtilityFunctions.{eyeColourMatch, formatValForRemove, genderMatch, hairColourMatch}
import scala.collection.mutable

object GamePlay extends App {

  // Map of the individual Characters
  val individualsMap: Map[Int, Character] = Map(
    1 -> Individuals.p1,
    2 -> Individuals.p2,
    3 -> Individuals.p3,
    4 -> Individuals.p4,
    5 -> Individuals.p5,
    6 -> Individuals.p6,
    7 -> Individuals.p7,
    8 -> Individuals.p8,
    9 -> Individuals.p9,
    10 -> Individuals.p10,
    11 -> Individuals.p11,
    12 -> Individuals.p12,
    13 -> Individuals.p13,
    14 -> Individuals.p14,
    15 -> Individuals.p15,
    16 -> Individuals.p16,
    17 -> Individuals.p17,
    18 -> Individuals.p18,
    19 -> Individuals.p19,
    20 -> Individuals.p20,
    21 -> Individuals.p21,
    22 -> Individuals.p22,
    23 -> Individuals.p23,
    24 -> Individuals.p24
  )

  // Finding a random character for you to guess
  val characterToGuess: Character = UtilityFunctions.randomChars(individualsMap)

  // exit boolean, when true the game ends

  // Remaining Characters on the virtual game board
  var remainingCharacters : Map [Int, Character] = individualsMap

  // Accepted strings to use for pattern matching
  val acceptedStrings: Seq[String] = Seq("gender", "glasses", "facialhair", "hat", "eyecolour", "hashair", "haircolour") // This order

  // Accepted Strings for pattern matching that have not been used yet
  var remainingFeatures: Seq[String] = Seq("hashair", "glasses", "facialhair", "eyecolour", "eyecolour", "eyecolour", "haircolour", "hat", "gender", "haircolour", "haircolour", "haircolour")

  // Remaining Hair Options
  var remainingHairColours: Seq[String] = Seq("1. brown", "2. blonde", "3. black")

  // Remaining Eye Options
  var remainingEyeColours: Seq[String] = Seq("1. brown", "2. blue", "3. green")

  def gameLoop(exit: Boolean = false): Unit = {

    while(!exit) {

      // Creates a Set out of the remaining Q's, Removes duplicates for us to print
      val remainingQuestions: Set[String] = remainingFeatures.toSet

      // Tells you what questions are still to be asked
      UtilityFunctions.gameQuestions(remainingQuestions, acceptedStrings)

      // reads user response for main category
      var response: String = scala.io.StdIn.readLine().toLowerCase().trim() // to lower case

      // In Game Options
      response match {
        case "b" =>
          UtilityFunctions.printAllRemainingChars(remainingCharacters: Map[Int, Character])
          gameLoop()
        case "h" =>
          UtilityFunctions.helpMe()
          gameLoop()
        case "e" =>
          gameLoop(exit = true)
        case "r" =>
          println("Rules")
        case "g" =>
          UtilityFunctions.printAllRemainingChars(remainingCharacters: Map[Int, Character])
          println("Which character to you think is the undercover agent?")
          println("Be careful, get it wrong and the game ends!")
          println("To exit guess, type 'X'")
          val guess: String = scala.io.StdIn.readLine().toLowerCase().trim()
          guess match {
            case "x" => gameLoop()
            case char if (characterToGuess.name.toLowerCase() == char) =>
              def gameWin (): Unit = {
                println("🥳🎉 Congratulations!! 🥳🎉")
                Thread sleep 2000
                gameLoop(exit = true)
              }
              gameWin()
            case _ =>
              def gameLose (): Unit = {
                println("Boooo you suck!!")
                Thread sleep 2000
                gameLoop(exit = true)
              }
              gameLose()
          }
        case _ => println("")
      }

      response = response match {
        case "1" => "gender"
        case "2" => "glasses"
        case "3" => "facialhair"
        case "4" => "hat"
        case "5" => "eyecolour"
        case "6" => "hashair"
        case "7" => "haircolour"
        case _ => ""
      }

      // Checks if the response is still valid i.e. if you haven't asked already
      if (!remainingFeatures.contains(response) && acceptedStrings.contains(response)) {

        val YELLOW = "\u001B[33m"
        val BOLD = "\u001B[1m"
        val RESET = "\u001B[0m"

        println(s"\n${YELLOW}${BOLD}You have already asked this question, try again!${RESET} \n")
        gameLoop()
      }
      // if the user puts in an invalid string/input restart the turn
      else if (!acceptedStrings.contains(response)) {

        val RED = "\u001B[31m"
        val BOLD = "\u001B[1m"
        val RESET = "\u001B[0m"

        println(s"\n${RED}${BOLD}That selection is not available please try again!${RESET} \n")
        gameLoop()
      }
      // initialised for subcategory i.e. colour
      var value: Any = ""
      var include: Boolean = true

      // based on the primary response allows you to pick the sub category
      response match {
        // When Your primary selection is haircolour
        case "haircolour" => {
          value = hairColourMatch(response, remainingHairColours)
          if (characterToGuess.hairColour == value) include = true else include = false
        }
        case "eyecolour" => {
          value = eyeColourMatch(response: String, remainingEyeColours: Seq[String])
          if (characterToGuess.eyeColour == value) include = true else include = false
        }
        case "gender" => {
          value = genderMatch()
          if (characterToGuess.gender == value) include = true else include = false
        }
        case "glasses" => {
          value = characterToGuess.glasses
        }
        case "facialhair" => {
          value = characterToGuess.facialHair
        }
        case "hat" => {
          value = characterToGuess.hat
        }
        case "hashair" => {
          value = characterToGuess.hasHair
        }
      }

      // This creates a filtered list of characters based on a feature - returns all people with hats
      val updatedCharacters: Map[Int, Character] = UtilityFunctions.playRound(individualsMap: Map[Int, Character], response, value)

      // This will eventually check character to Guess!!!!
      if (include) {
        println(s"The mystery character does have ${response} ${value}, good option!")
        if (response == "hashair" && !characterToGuess.hasHair) {
          remainingFeatures = remainingFeatures.filterNot(_ == "haircolour")
        }
        // This filters the remaining characters in the game board to get rid of those that match the criteria
        remainingCharacters = remainingCharacters.filter {
          case (key, _) => updatedCharacters.contains(key)
        }
      } else if (!include) {
        println(s"Uh Oh... The mystery character does not have ${response} ${value}!")
        // This filters the remaining characters in the game board to get rid of those that match the criteria
        remainingCharacters = remainingCharacters.filter {
          case (key, _) => !updatedCharacters.contains(key)
        }
      } else {
        // this is error handling
        println("error message here")
        gameLoop()
      }

      if (remainingFeatures.contains(response)) {
        // Once used, the feature is then found in the Seq and removed so it cannot be searched for again
        val index = remainingFeatures.indexOf(response)
        remainingFeatures = remainingFeatures.patch(index, Nil, 1)


        def updateRemainingColours(response: String, value: Any): Unit = {

          val formattedValue = formatValForRemove(value)

          if (response == "haircolour") {
            remainingHairColours = remainingHairColours.filterNot(_ == formattedValue)
          } else if (response == "eyecolour") {
            remainingEyeColours = remainingEyeColours.filterNot(_ == formattedValue)
          }
        }

        updateRemainingColours(response, value)
      }



      val numOfChar: Int = remainingCharacters.size
      println(s"${numOfChar} out of 24 characters left on the board. \n")

      def allowGuessing ()= {

        val BOLD = "\u001B[1m"
        val UNDERLINE = "\u001B[4m"
        val RESET = "\u001B[0m"
        val PURPLE = "\u001B[35m"

        Thread.sleep(2000)
        if (remainingCharacters.size == 1) {
          println(s"Ahhh the character is ${characterToGuess.name}!  \n")
          gameLoop(true)
        } else if (remainingCharacters.size <= 5) {
          println (s"${BOLD}${UNDERLINE}${PURPLE}Do you want to take a guess now? Press 'G' or just continue with the game. ${RESET} \n")
        } else {
          gameLoop()
        }
      }

      allowGuessing()

    }

    println("Game Ended")
    Thread.sleep(4000)
    // clear the console - GREP CONSOLE IF WANTED TO IN FUTURE
    startGame()
  }


  def startGame(): Unit = {
    println("Type Start to Start")
    val response: String = scala.io.StdIn.readLine().toLowerCase()
    if (response == "start") {
      UtilityFunctions.intro()
      UtilityFunctions.initialHelpRules()
      UtilityFunctions.goodLuck()
      gameLoop()
    }
  }

  startGame()

}