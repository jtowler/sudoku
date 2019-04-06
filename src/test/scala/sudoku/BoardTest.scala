package sudoku

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
    val onTest = Board(tiles)
    val onTestTranspose = Board(tiles)
    onTest.validate() shouldBe false
    onTestTranspose.validate() shouldBe false
  }

  it should "correctly detect invalid board with multiple labels in a square" in {
    val emptyLine = List.tabulate(9)(_ => ' ')
    val badLine1 = List.tabulate(9)(i => if (i == 7) '1' else ' ')
    val badLine2 = List.tabulate(9)(i => if (i == 8) '1' else ' ')
    val onTest = Board(List.tabulate(9){i =>
      if (i == 7) badLine1
      else if (i == 8) badLine2
      else emptyLine})
    onTest.validate() shouldBe false
  }

  behavior of "hasWon"
  it should "identify a complete and valid board" in {
    val onTest = Board(Examples.complete)
    onTest.hasWon shouldBe true
  }

  behavior of "update"
  it should "update value of tiles" in {
    val onTest = Board().update(1, 3, '2').update(4, 7, '3')
    onTest.tiles(3)(1) shouldBe '2'
    onTest.tiles(7)(4) shouldBe '3'
  }

  it should "update an already updated tile" in {
    val onTest = Board().update(1, 3, '2')
    onTest.tiles(3)(1) shouldBe '2'
    val onTest2 = onTest.update(1, 3, '3')
    onTest2.tiles(3)(1) shouldBe '3'
  }

  it should "not update initial tiles" in {
    val emptyLine = List.tabulate(9)(_ => ' ')
    val nonEmptyLine = List.tabulate(9)(i => if (i == 7) '1' else ' ')
    val onTest = Board(List.tabulate(9){i =>
      if (i == 7) nonEmptyLine
      else emptyLine})
    onTest.tiles(7)(7) shouldBe '1'
    val onTest2 = onTest.update(7, 7, '2')
    onTest2.tiles(7)(7) shouldBe '1'
  }

  it should "remove tiles if given a non digit character" in {
    val onTest = Board().update(1, 3, '2')
    onTest.tiles(3)(1) shouldBe '2'
    val onTest2 = onTest.update(1, 3, 'a')
    onTest2.tiles(3)(1) shouldBe ' '
  }

}
