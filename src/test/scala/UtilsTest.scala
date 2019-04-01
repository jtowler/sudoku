import org.scalatest.{FlatSpec, Matchers}

class UtilsTest extends FlatSpec with Matchers {

  behavior of "fromSquares"

  it should "return squares as lists and back again" in {
    val intermediate = Utils.fromSquares(Examples.complete)
    val restored = Utils.fromSquares(intermediate)
    restored shouldBe Examples.complete

  }

}
