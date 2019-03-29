import org.scalatest.{FlatSpec, Matchers}

class BoardTest extends FlatSpec with Matchers {

  behavior of "validate"
  it should "identify an empty board as valid" in {
    val onTest = Board()
    onTest.validate() shouldBe true
  }

  it should "correctly detect invalid board" in {
    val emptyLine = List.tabulate(9)(_ => ' ')
    val badLine = List.tabulate(9)(i => if (i > 2) ' ' else '1')
    val tiles = List.tabulate(9)(i => if (i == 5) badLine else emptyLine)
    val onTest = new Board(tiles)
    val onTestTranspose = new Board(tiles)
    onTest.validate() shouldBe false
    onTestTranspose.validate() shouldBe false
  }

  it should "correctly detect invalid board with multiple labels in a square" in {
    val emptyLine = List.tabulate(9)(_ => ' ')
    val badLine1 = List.tabulate(9)(i => if (i == 7) '1' else ' ')
    val badLine2 = List.tabulate(9)(i => if (i == 8) '1' else ' ')
    val onTest = new Board(List.tabulate(9){i =>
      if (i == 7) badLine1
      else if (i == 8) badLine2
      else emptyLine})
    onTest.validate() shouldBe false
  }

  behavior of "update"
  it should "update value of tiles" in {
    val onTest = Board().update(1, 3, '2').update(4, 7, '3')
    onTest.tiles(3)(1) shouldBe '2'
    onTest.tiles(7)(4) shouldBe '3'
  }

}
