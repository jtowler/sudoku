import org.scalatest.{FlatSpec, Matchers}

class UtilsTest extends FlatSpec with Matchers {

  behavior of "fromSquares"

  it should "return squares as lists and back again" in {
    val intermediate = Utils.fromSquares(Examples.complete)
    val restored = Utils.fromSquares(intermediate)
    restored shouldBe Examples.complete
  }

  behavior of "getSquareFlat"
  it should "get the values in a square at a given point in a flat list" in {
    val onTest = Utils.getSquare(Examples.complete, 5, 7)
    val result = List('5', '3', '7', '4', '1', '9', '2', '8', '6')
    onTest shouldBe result

  }

}
