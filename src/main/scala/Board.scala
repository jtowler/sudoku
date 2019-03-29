class Board(val tiles: List[List[Char]]) {

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

    print(dispTiles)

  }

  def validate(): Boolean = {

    def check(toCheck: List[List[Char]]): Boolean = toCheck.forall {
      line =>
        Utils.labels.forall {
          label =>
            line.count(_ == label) <= 1
        }
    }

    lazy val squares = for {
      x <- 0 to 6 by 3
      y <- 0 to 6 by 3
    } yield tiles.slice(x, x + 3).slice(y, y + 3).flatten

    check(tiles) && check(tiles.transpose) && check(squares.toList)

  }

}

object Board {
  def apply(): Board =
    new Board(
      List.tabulate(9, 9)((_, _) => ' ')
  )
}