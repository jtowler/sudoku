class Board(val tiles: List[List[Char]], val fixedTiles: List[List[Char]], val autoValidate: Boolean = false) {

  lazy val squares: List[List[Char]] = Utils.fromSquares(tiles)

  def solve: Board = new Board(SimpleSolver.solveAsMuchAsPossible(tiles), fixedTiles, autoValidate)
  def solveOnce: Board = new Board(SimpleSolver.solveOnce(tiles), fixedTiles, autoValidate)

  def display(): Unit = {

    val dispTiles: String = tiles.grouped(3).map {
      triple =>
        triple.map {
          line =>
            line.grouped(3)
              .map(_.mkString("")).mkString("#")
        }
    }.map {
      triple => triple.mkString("\n")
    }.mkString("\n###########\n")

    println(dispTiles)

  }

  def hasWon: Boolean = {

    def check(toCheck: List[List[Char]]): Boolean = toCheck.forall {
      line =>
        Utils.labels.forall {
          label =>
            line.count(_ == label) == 1
        }
    }

    validate() && check(tiles) && check(tiles.transpose) && check(squares)

  }

  def validate(): Boolean = {

    def check(toCheck: List[List[Char]]): Boolean = toCheck.forall {
      line =>
        Utils.labels.forall {
          label =>
            line.count(_ == label) <= 1
        }
    }

    check(tiles) && check(tiles.transpose) && check(squares)

  }

  def update(x: Int, y: Int, v: Char): Board = {
    val c = if (v.isDigit) v else ' '
    if (fixedTiles(y)(x) != ' ') this
    else {
      val newBoard = new Board(
        List.tabulate(9, 9) {
          case (b, a) =>
            if (b == y && a == x) c
            else tiles(b)(a)
        },
        fixedTiles,
        autoValidate
      )
      if (autoValidate && !newBoard.validate())
        this
      else
        newBoard
    }
  }

}

object Board {
  def apply(): Board = Board(List.tabulate(9, 9)((_, _) => ' '))

  def apply(tiles: List[List[Char]]): Board = {
    new Board(tiles, tiles)
  }

  def apply(tiles: List[List[Char]], autoValidate: Boolean): Board = {
    new Board(tiles, tiles, autoValidate)
  }
}