object UtilityFunctions {

  def randomChars (individualsMap: Map[Int, Character]): Character = {
    val randomCharacter = new scala.util.Random
    val characterToGuess = individualsMap(randomCharacter.nextInt(24) + 1)
    characterToGuess
  }

}
