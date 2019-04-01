object Utils {

  val labels = List('1', '2', '3', '4', '5', '6', '7', '8', '9')

  def fromSquares[A](xs: List[List[A]]): List[List[A]] = (for {
    x <- 0 to 6 by 3
    y <- 0 to 6 by 3
  } yield xs.slice(x, x + 3).flatMap(_.slice(y, y + 3))).toList

}
