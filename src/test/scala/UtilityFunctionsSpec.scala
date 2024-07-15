import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

case class UtilityFunctionsSpec () extends AnyWordSpec {

  val uf = new UtilityFunctions

  class TestClass {
    val oneVal = "1"
    val twoVal = "2"
  }

  object TestObject {
    val oneTesting = new TestClass
    val twoTesting = new TestClass
  }

  "" should {
    "" when {
      "" in {
        ???
      }
    }
  }
}
