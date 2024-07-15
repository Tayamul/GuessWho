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
        case "hat" => hat == value
        case "eyecolour" => eyeColour == value
        case "hair" => true == value
        case "gender" => "female" == value
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
  val acceptedStrings: Seq[String] = Seq("hair", "glasses", "facialhair", "eyecolour", "haircolour", "hat", "gender")
  var remainingFeatures: Seq[String] = Seq("hair", "glasses", "facialhair", "eyecolour", "eyecolour", "eyecolour", "haircolour", "hat", "gender", "haircolour", "haircolour", "haircolour", "eyecolour")

  var remainingHairColours: Seq[String] = Seq("red", "brown", "blonde", "black", "grey")
  var remainingEyeColours: Seq[String] = Seq("blue", "brown", "green")

  def gameLoop( ): Unit = {
  do {

    val remainingQuestions: Set[String] = remainingFeatures.toSet


    println("You have the remaining characteristics to choose from:")
    remainingQuestions.foreach( feature => println(feature))


    var response : String = scala.io.StdIn.readLine().toLowerCase() // to lower case
    println(response)

    var value: Any = ""

    response match {
      case "haircolour" => {
        println("Which colour hair:")
        remainingHairColours.foreach {
          case (element) => print(f"$element ")
        }
        value = scala.io.StdIn.readLine().toLowerCase()
      }
      case "eyecolour" => {
        println("Which colour eye:")
        remainingEyeColours.foreach {
          case (element) => print(f"$element" + "\n")
        }
        value = scala.io.StdIn.readLine().toLowerCase()
      }
      case "gender" => {
        println("Male or Female?")
        value = scala.io.StdIn.readLine().toLowerCase()
      }
      case "glasses" => {
        value = true
      }
      case "facialhair" => {
        value = true
      }
      case "hat" => {
        value = true
      }
      case "hair" => {
        value = true
      }
    }

    if (acceptedStrings.contains(response)) {

    // This creates a filtered list of characters based on a feature - returns all people with hats

      var updatedCharacters = playRound(individualsMap: Map [Int, Character], response, value)
      println(updatedCharacters)
      remainingCharacters = remainingCharacters.filter {
        case (key, _) => updatedCharacters.contains(key)
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