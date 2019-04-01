object SimpleSolver {

  def fillInOnes(tiles: List[List[Char]]): List[List[Char]] = {
    val intermediate = tiles.map(fillInOne).transpose.map(fillInOne).transpose
    val squared = Utils.fromSquares(intermediate)
    Utils.fromSquares(squared.map(fillInOne))
  }

  def fillInOne(tiles: List[Char]): List[Char] = {
    val missingVals = Utils.labels.filterNot(tiles contains _ )
    if (missingVals.size == 1)
      tiles.map(c => if (c == ' ') missingVals.head else c)
    else
      tiles
  }

}
