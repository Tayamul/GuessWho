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
  val characterToGuess = UtilityFunctions.randomChars(individualsMap)

  // exit boolean, when true the game ends

  // Remaining Characters on the virtual game board
  var remainingCharacters : Map [Int, Character] = individualsMap

  // Accepted strings to use for pattern matching
  val acceptedStrings: Seq[String] = Seq("hair", "glasses", "facialhair", "eyecolour", "haircolour", "hat", "gender")

  // Accepted Strings for pattern matching that have not been used yet
  var remainingFeatures: Seq[String] = Seq("hair", "glasses", "facialhair", "eyecolour", "eyecolour", "eyecolour", "haircolour", "hat", "gender", "haircolour", "haircolour", "haircolour")

  // Remaining Hair Options
  var remainingHairColours: Seq[String] = Seq("red", "brown", "blonde", "black", "grey")

  // Remaining Eye Options
  var remainingEyeColours: Seq[String] = Seq("blue", "brown", "green")

  def gameLoop(exit: Boolean = false): Unit = {

    while(!exit) {

      // Creates a Set out of the remaining Q's, Removes duplicates for us to print
      val remainingQuestions: Set[String] = remainingFeatures.toSet

      // Tells you what questions are still to be asked
      UtilityFunctions.gameQuestions(remainingQuestions)

      // reads user response for main category
      var response: String = scala.io.StdIn.readLine().toLowerCase() // to lower case

      // In Game Options
      if (response == "b") {
        UtilityFunctions.printAllRemainingChars(remainingCharacters: Map[Int, Character])
        gameLoop()
      } else if (response == "h") {
        UtilityFunctions.helpMe()
        gameLoop()
      } else if (response == "e") {
        gameLoop(exit = true)
      } else if (response == "r") {
        println("Rules")
      } else if (response == "g") {
        UtilityFunctions.printAllRemainingChars(remainingCharacters: Map[Int, Character])
        println("Which character to you think is the undercover agent?")
        println("Be careful, get it wrong and the game ends!")
        val guess: String = scala.io.StdIn.readLine().toLowerCase()
        if(characterToGuess.name.toLowerCase() == guess) {

          def gameWin (): Unit = {
            println("🥳🎉 Congratulations!! 🥳🎉")
            Thread sleep 2000
            gameLoop(exit = true)
          }

          gameWin()
        } else {

          def gameLose (): Unit = {
            println("Boooo you suck!!")
            Thread sleep 2000
            gameLoop(exit = true)
          }

          gameLose()
        }
      }


      // Checks if the response is still valid i.e. if you haven't asked already
      if (!remainingFeatures.contains(response) && acceptedStrings.contains(response)) {
        println("you have already asked this question, try again")
        gameLoop()
      }
      // if the user puts in an invalid string/input restart the turn
      else if (!acceptedStrings.contains(response)) {
        println("That selection is not available please try again")
        gameLoop()
      }
      // initialised for subcategory i.e. colour
      var value: Any = ""
      var include: Boolean = true

      // based on the primary response allows you to pick the sub category
      response match {
        // When Your primary selection is haircolour
        case "haircolour" => {
          println("Which colour hair:")
          remainingHairColours.foreach {
            case (element) => print(f"$element ")
          }
          value = scala.io.StdIn.readLine().toLowerCase()
          if (characterToGuess.hairColour == value) include = true else include = false
        }
        case "eyecolour" => {
          println("Which colour eye:")
          remainingEyeColours.foreach {
            case (element) => print(f"$element" + "\n")
          }
          value = scala.io.StdIn.readLine().toLowerCase()
          if (characterToGuess.eyeColour == value) include = true else include = false
        }
        case "gender" => {
          println("Male or Female?")
          value = scala.io.StdIn.readLine().toLowerCase()
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
        case "hair" => {
          value = characterToGuess.hasHair
        }
      }

      // This creates a filtered list of characters based on a feature - returns all people with hats
      val updatedCharacters: Map[Int, Character] = UtilityFunctions.playRound(individualsMap: Map[Int, Character], response, value)

      // This will eventually check character to Guess!!!!
      if (include) {
        println(s"The mystery character does have ${response} ${value}, good option!")
        if (response == "hair" && !characterToGuess.hasHair) {
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
      }

      // if filtering by a string value
      if (response == "haircolour") {
        // filter out the used value
        remainingHairColours = remainingHairColours.filterNot(_ == value)
      } else if (response == "eyecolour") {
        // filter out the used value
        remainingEyeColours = remainingEyeColours.filterNot(_ == value)
      } else {

      }


      val numOfChar: Int = remainingCharacters.size
      println(s"${numOfChar} out of 24 characters left on the board.")


    }

    println("Game Ended")
    Thread.sleep(4000)
    // clear the console - GREP CONSOLE IF WANTED TO IN FUTURE
    startGame()
  }

  val BG_BRIGHT_WHITE = "\u001B[107m"
  val BLACK = "\u001B[30m"
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
  val BG_BRIGHT_GREEN = "\u001B[102m"
  val BG_BRIGHT_YELLOW = "\u001B[103m"

  def intro (): Unit = {
    println(s"${BG_BRIGHT_YELLOW}${BLACK}${BOLD}${UNDERLINE}!! * Ultimate Guess Who Experience * !!${RESET}")
    println()
    println(s"${BG_BRIGHT_GREEN}${BLACK}${BOLD}The rules are simple ....  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla placerat magna eget leo condimentum, sit amet porttitor ex congue. Proin dolor lorem, scelerisque in quam ut, finibus efficitur ligula. Donec pharetra tempor neque mattis tincidunt. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Aenean dictum convallis iaculis. Duis rhoncus imperdiet velit. Phasellus ultricies nisl sit amet ipsum tempor dapibus.${RESET}")
    println()

  }


  def initialHelpRules (): Unit = {
    println(s"${BG_BRIGHT_GREEN}${BLACK}${BOLD}Whenever you are prompted for an answer you can also access the following commands:" +
      s"\n Enter B -> To see the remaining characters on the board and their attributes \n Enter G -> To make a guess at who the character is \n Enter E -> To leave the game :( \n Enter R -> To see the rules and how to win  \n Enter H -> At any time to see these options again ${RESET}")
    println()
  }



  def goodLuck (): Unit = {
    println(s"${BG_BRIGHT_WHITE}${BOLD}${UNDERLINE}${BLUE}G${RED}O${GREEN}O${YELLOW}D ${BLUE}L${PURPLE}U${CYAN}C${WHITE}K${RESET}")
    println()
  }




  def startGame(): Unit = {
    println("Type Start to Start")
    val response: String = scala.io.StdIn.readLine().toLowerCase()
    if (response == "start") {
      intro()
      initialHelpRules()
      goodLuck()
      gameLoop()
    }
  }

  startGame()

}