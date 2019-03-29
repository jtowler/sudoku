object Main extends App {

  val board = Board()
  val board2 = board.update(0, 3, '2')
  board2.display()

}
