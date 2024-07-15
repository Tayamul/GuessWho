import scala.collection.mutable

object GamePlay extends App {

  val individualsMap: Map[Int, Any] = Map(
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
  def matchCharacteristics(individualsMap: Map[Int, Any], feature: String, value: Any): Map[Int, Any] = {
    individualsMap.filter {
      case (_, BaldMan(name, glasses, facialHair, hat, eyeColour)) => feature match {
        case "name" => name == value
        case "glasses" => glasses == value
        case "facialHair" => facialHair == value
        case "hat" => hat == value
        case "eyeColour" => eyeColour == value
        case _ => true
      }
      case (_, HairMan(name, glasses, facialHair, hairColour, hat, eyeColour)) => feature match {
        case "name" => name == value
        case "glasses" => glasses == value
        case "facialHair" => facialHair == value
        case "hairColour" => hairColour == value
        case "hat" => hat == value
        case "eyeColour" => eyeColour == value
        case _ => true
      }
      case (_, Female(name, glasses, hairColour, hat, eyeColour)) => feature match {
        case "name" => name == value
        case "glasses" => glasses == value
        case "hairColour" => hairColour == value
        case "hat" => hat == value
        case "eyeColour" => eyeColour == value
        case _ => true
      }
      case _ => false
    }
  }

  def playRound(characters: Map[Int, Any], feature: String, value: Any): Map[Int, Any] = {
    val remainingCharacters = matchCharacteristics(characters: Map
      [Int, Any], feature, value)
    remainingCharacters
  }

  val updatedCharacters = playRound(individualsMap: Map[Int, Any], "name", "John")

}
