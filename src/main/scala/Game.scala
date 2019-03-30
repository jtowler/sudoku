import scala.io.StdIn

object Game {

  def loop(board: Board): Unit = {

    board.display()

    val input = StdIn.readLine("Enter move: ")
    val inputs = input.split(" ")
    val x = inputs(0).toInt
    val y = inputs(1).toInt
    val v = inputs(2).head
    val newBoard = board.update(x, y, v)
    if (!newBoard.hasWon) loop(newBoard)

  }

}
