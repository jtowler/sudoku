import org.scalatest.{FlatSpec, Matchers}

class SimpleSolverTest extends FlatSpec with Matchers {

  behavior of "fillInOne"
  it should "complete a list of characters with one missing value" in {
    val initList = List('1','2','3','4','5','6','7','8',' ')
    val onTest = SimpleSolver.fillInOne(initList)
    val result = (1 to 9).map(_.toString.charAt(0)).toList
    onTest shouldBe result
  }


  behavior of "fillInOnes"
  it should "complete a board of characters with rows that have one missing value" in {
    val initList = Examples.complete.zipWithIndex.map{
      case (line, i) =>
        line.zipWithIndex.map{
          case (tile, j) =>
            if ((i, j) == (1, 2) || (i, j) == (2, 2)) ' '
            else tile
        }
    }
    val onTest = SimpleSolver.fillInOnes(initList)
    onTest shouldBe Examples.complete
  }


  behavior of "fillInOnes"
  it should "complete a board of characters with squares that have one missing value" in {
    val initList = Examples.complete.zipWithIndex.map{
      case (line, i) =>
        line.zipWithIndex.map{
          case (tile, j) =>
            if ((i, j) == (1, 2) || (i, j) == (7, 2)) ' '
            else tile
        }
    }
    val onTest = SimpleSolver.fillInOnes(initList)
    onTest shouldBe Examples.complete
  }

  behavior of "fillInMissing"
  it should "complete a board of characters with squares that have one missing value" in {
    val onTest = SimpleSolver.fillInMissing(Examples.easy, 6, '4')
    val result = '4'+:Examples.easy(6).tail
    onTest shouldBe result
  }
//
//  behavior of "fillInAllMissings"
//  it should "complete a board of characters as much as possible" in {
//    val onTest = SimpleSolver.fillInAllMissings(Examples.easy)
//    println(onTest)
//  }
//
//  behavior of "solveAsMuchAsPossible"
//  it should "complete a board of characters as much as possible" in {
//    val onTest = SimpleSolver.solveAsMuchAsPossible(Examples.easy)
//    println(onTest)
//  }


}
