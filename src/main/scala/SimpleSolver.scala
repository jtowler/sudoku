object SimpleSolver {

  def fillInOnes(tiles: List[List[Char]]): List[List[Char]] = {
    val intermediate = tiles.map(fillInOne).transpose.map(fillInOne).transpose
    val squared = Utils.fromSquares(intermediate)
    Utils.fromSquares(squared.map(fillInOne))
  }

  def fillInOne(tiles: List[Char]): List[Char] = {
    val missingVals = Utils.labels.filterNot(tiles contains _)
    if (missingVals.size == 1)
      tiles.map(c => if (c == ' ') missingVals.head else c)
    else
      tiles
  }

  def solveAsMuchAsPossible(tiles: List[List[Char]]): List[List[Char]] = {
    val intermediateTiles = fillInAllMissings(tiles)
    val newTiles = fillInAllMissings(intermediateTiles.transpose).transpose
    if (newTiles == tiles) newTiles
    else solveAsMuchAsPossible(newTiles)
  }

  def fillInAllMissings(tiles: List[List[Char]], row: Int = 0): List[List[Char]] = row match {
    case x if x >= 9 => tiles
    case _ => fillInAllMissings(fillInMissings(tiles, row), row + 1)
  }

  def fillInMissings(tiles: List[List[Char]], row: Int): List[List[Char]] = {

    def loop(currLine: List[Char], missVals: List[Char]): List[Char] = missVals match {
      case Nil => currLine
      case h :: t => loop(fillInMissing(tiles, row, h), t)
    }

    val line = tiles(row)
    val missingVals = Utils.labels.filterNot(line contains _)

    val newLine = loop(line, missingVals)
    tiles.zipWithIndex.map{ case (t, i) => if (i == row) newLine else t}
  }


  def fillInMissing(tiles: List[List[Char]], row: Int, value: Char): List[Char] = {
    val line = tiles(row)
    val spaces = line.zipWithIndex.filter { case (tile, _) => tile == ' ' }.map(_._2)
    val potentialSpaces = spaces.filterNot(i => {
      tiles.transpose.toList(i).contains(value) || Utils.getSquare(tiles, i, row).contains(value)
    })
    if (potentialSpaces.size == 1) line
      .zipWithIndex
      .map {
        case (t, i) => if (i == potentialSpaces.head) value else t
      }
    else line
  }

}
