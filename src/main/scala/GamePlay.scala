import scala.collection.mutable

object GamePlay extends App {

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

  val randomCharacter = new scala.util.Random
  val characterToGuess = individualsMap(randomCharacter.nextInt(25))

  // pattern matching
  def matchCharacteristics(individualsMap: Map[Int, Character], feature: String, value: Any): Map[Int, Character] = {
    individualsMap.filter {
      case (_, BaldMan(name, glasses, facialHair, hat, eyeColour)) => feature match {
        case "name" => name == value
        case "glasses" => glasses == value
        case "facialHair" => facialHair == value
        case "hat" => hat == value
        case "eyeColour" => eyeColour == value
        case "hair" => false == value
        case _ => true
      }
      case (_, HairMan(name, glasses, facialHair, hairColour, hat, eyeColour)) => feature match {
        case "name" => name == value
        case "glasses" => glasses == value
        case "facialHair" => facialHair == value
        case "hairColour" => hairColour == value
        case "hat" => hat == value
        case "eyeColour" => eyeColour == value
        case "hair" => true == value
        case _ => true
      }
      case (_, Female(name, glasses, hairColour, hat, eyeColour)) => feature match {
        case "name" => name == value
        case "glasses" => glasses == value
        case "hairColour" => hairColour == value
        case "hat" => hat == value
        case "eyeColour" => eyeColour == value
        case "hair" => true == value
        case _ => true
      }
      case _ => false
    }
  }

  def playRound(characters: Map[Int, Character], feature: String, value: Any): Map[Int, Character] = {
    val remainingCharacters = matchCharacteristics(characters: Map
      [Int, Character], feature, value)
    remainingCharacters
  }

  var exit: Boolean = false

  var remainingCharacters : Map [Int, Character] = individualsMap

  // Accepted Strings as a feature in a set
  val acceptedStrings: Seq[String] = Seq("hair", "glasses", "facialHair", "eyeColour", "hairColour", "hat", "gender")
  var remainingFeatures: Seq[String] = Seq("hair", "glasses", "facialHair", "eyeColour", "eyeColour", "eyeColour", "hairColour", "hat", "gender", "hairColour", "hairColour", "hairColour", "eyeColour")


  def gameLoop( ): Unit = {
  do {

    val remainingQuestions: Set[String] = remainingFeatures.toSet


    println("You have the remaining characteristics to choose from:")
    remainingQuestions.foreach( feature => println(feature))


    var response : String = scala.io.StdIn.readLine().toLowerCase() // to lower case

    if (acceptedStrings.contains(response)) {

    // This creates a filtered list of characters based on a feature - returns all people with hats

      var updatedCharacters = playRound(individualsMap: Map [Int, Character], response, true)
      println(updatedCharacters)
      remainingCharacters = remainingCharacters.filter {
        case (key, _) => !updatedCharacters.contains(key)
      }

    } else {
      println("error message here")
      gameLoop()
    }


    // Does our mystery character have a hat

    // if yes leave all people in list

    // if no update our list:

    remainingCharacters.foreach(println)

        if (remainingCharacters.size == 1) {
          exit = true
        }

  } while (!exit)

  println("Game Ended")
  }

  gameLoop()
}