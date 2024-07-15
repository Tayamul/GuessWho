object UtilityFunctions {

  // Finding a random character for you to guess
  def randomChars (individualsMap: Map[Int, Character]): Character = {
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

}
