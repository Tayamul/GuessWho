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
  val randomCharacter = new scala.util.Random
  val characterToGuess = individualsMap(randomCharacter.nextInt(24) + 1)

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
      println("You have the remaining characteristics to choose from:")
      remainingQuestions.foreach(feature => println(feature))

      // reads user response for main category
      var response: String = scala.io.StdIn.readLine().toLowerCase() // to lower case

      val RESET = "\u001B[0m"
      val BG_WHITE = "\u001B[47m"
      val BLACK = "\u001B[30m"
      val BG_BRIGHT_GREEN = "\u001B[102m"
      val BG_BRIGHT_YELLOW = "\u001B[103m"
      val BOLD = "\u001B[1m"


      def printAllChars(): Unit = {
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
        println(s"${BG_BRIGHT_GREEN}${BLACK}${BOLD} Welcome to the Help Menu: \n Enter B -> To see the remaining characters on the board and their attributes \n Enter G -> To make a guess at who the character is \n Enter E -> To leave the game :( \n Enter R -> To see the rules and how to win  \n Enter H -> At any time for help ... As you know...  ${RESET}")
      }


      // In Game Options
      if (response == "b") {
        printAllChars()
        gameLoop()
      } else if (response == "h") {
        helpMe()
        gameLoop()
      } else if (response == "e") {
        gameLoop(exit = true)
      } else if (response == "r") {
        println("Rules")
      } else if (response == "g") {
        println("Guess")
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
      val updatedCharacters: Map[Int, Character] = playRound(individualsMap: Map[Int, Character], response, value)

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