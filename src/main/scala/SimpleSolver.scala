object SimpleSolver {

//  def solve(board: Board): Board = {
//    if (board.hasWon) board
//    else {
//      board.tiles.zipWithIndex.foreach{
//        case (line, index) =>
//          line.foreach()
//
//      }
//    }
//  }

  def fillInOnes(tiles: List[List[Char]]): List[List[Char]] = {
    tiles.map(fillInOne).transpose.map(fillInOne).transpose
  }

  def fillInOne(tiles: List[Char]): List[Char] = {
    val missingVals = Utils.labels.filterNot(tiles contains _ )
    if (missingVals.size == 1)
      tiles.map(c => if (c == ' ') missingVals.head else c)
    else
      tiles
  }

}
