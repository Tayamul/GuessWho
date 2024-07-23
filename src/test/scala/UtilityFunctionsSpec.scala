import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec
import scala.reflect.runtime.universe._
case class UtilityFunctionsSpec () extends AnyWordSpec {

  val Jake: BaldMan = BaldMan(name = "Jake", glasses = true, facialHair = false, hat = true, eyeColour = "blue")
  val Anisha: Female = Female(name = "Anisha", glasses = true, hat = true, eyeColour = "blue", hairColour = "blonde")
  val Paul: HairMan = HairMan(name = "Paul", glasses = false, facialHair = false, hairColour = "brown", hat = false, eyeColour = "brown")

  val testDataSet = Map(
    1 -> Jake,
    2 -> Anisha,
    3 -> Paul,
  )

  "UtilityFunctions.randomChars" should {
    "return a random character from the character map" when {
      "called on a map of characters" in {
        def checkClass (person: Character): Boolean = {
          person match {
            case _: Female | _: BaldMan | _: HairMan => true
            case _ => false
          }
        }

        val result: Character = UtilityFunctions.randomChars(testDataSet, 3)
        assert(checkClass(result))
      }
    }
  }

  "UtilityFunctions.matchCharacteristics" should {
    "return all the characters that fit the characteristics" when {
      "an input is given for that particular feature" in {
        val result = UtilityFunctions.matchCharacteristics(testDataSet, "glasses", false)
        assert(result(3).name == "Paul")
      }
    }
    "return the size as one" when {
      "an input is given for that particular feature" in {
        val result = UtilityFunctions.matchCharacteristics(testDataSet, "glasses", false)
        assert(result.size == 1)
      }
    }
    "return character that matches the eye colour" when {
      "an input is provided" in {
        def checkCharacteristics (person: Character): Boolean = {
          person match {
            case p if p.eyeColour == "blue" => true
            case _ => false
          }
        }

        val result = UtilityFunctions.matchCharacteristics(testDataSet, "eyecolour", "blue")
        result.foreach {
          case (key, value) => assert(checkCharacteristics(value))
        }
      }
    }
  }

}
