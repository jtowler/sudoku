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

  def solveSquare(tiles: List[List[Char]], x: Int, y: Int, value: Char): List[List[Char]] = {
    val iX = (x / 3) * 3
    val iY = (y / 3) * 3
    val ys =  tiles.slice(iY, iY + 3).zipWithIndex.filter{case (line, _) => line contains value}.map(_._2)
    val xs =  tiles.transpose.slice(iX, iX + 3).zipWithIndex.filter{case (line, _) => line contains value}.map(_._2)
    if (xs.size == 1 && ys.size == 1) {
      tiles.zipWithIndex.map{
        case (line, j) => line.zipWithIndex.map{
          case (tile, i) =>
            if ((i, j) == (xs.head, ys.head)) value
            else tile
        }
      }
    }
    else
      tiles
  }

  def solveAllInSquare(tiles: List[List[Char]], x: Int, y: Int): List[List[Char]] = {

    def loop(currTiles: List[List[Char]], vals: List[Char]): List[List[Char]] = vals match {
      case Nil => currTiles
      case h :: t => loop(solveSquare(currTiles, x, y, h), t)
    }

    val missingVals = Utils.labels.filterNot(Utils.getSquareFlat(tiles, x, y) contains _)
    loop(tiles, missingVals)
  }

  def solveAllSquares(tiles: List[List[Char]]): List[List[Char]] = {

    def loop(currTiles: List[List[Char]], x: Int = 0, y: Int = 0): List[List[Char]] = (x, y) match {
      case (9, 9) => solveAllInSquare(currTiles, x, y)
      case (9, _) => loop(solveAllInSquare(currTiles, x, y), 0, y + 3)
      case _ => loop(solveAllInSquare(currTiles, x, y), x + 3, y)
    }

    loop(tiles)
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
      tiles.transpose.toList(i).contains(value) || Utils.getSquareFlat(tiles, i, row).contains(value)
    })
    if (potentialSpaces.size == 1) line
      .zipWithIndex
      .map {
        case (t, i) => if (i == potentialSpaces.head) value else t
      }
    else line
  }

}
