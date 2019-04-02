object Utils {

  val labels = List('1', '2', '3', '4', '5', '6', '7', '8', '9')

  def fromSquares[A](xs: List[List[A]]): List[List[A]] = (for {
    y <- 0 to 6 by 3
    x <- 0 to 6 by 3
  } yield xs.slice(y, y + 3).flatMap(_.slice(x, x + 3))).toList

  def getSquareFlat[A](xs: List[List[A]], x: Int, y: Int): List[A] = getSquare(xs, x, y).flatten

  def getSquare[A](xs: List[List[A]], x: Int, y: Int): List[List[A]] = {
    val iX: Int = (x / 3) * 3
    val iY: Int = (y / 3) * 3
    xs.slice(iY, iY  + 3).map(_.slice(iX, iX + 3))
  }

}
