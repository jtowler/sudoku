import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.layout.AnchorPane
import javafx.scene.shape.Line
import javafx.scene.text.Text
import javafx.stage.Stage

class UI extends Application {

  var board = Board(Examples.easy, autoValidate = true)
  val fullSize = 270
  val tileSize = 30

  private val labels = board.tiles.zipWithIndex.map { case (line, i) =>
    line.zipWithIndex.map { case (tile, j) =>
      new Text(j * tileSize + 11, i * tileSize + 18, tile.toString)
    }
  }

  private var clickX: Int = -1
  private var clickY: Int = -1

  override def start(primaryStage: Stage) {

    val root = new AnchorPane()
    root.setOnMouseClicked(x => {
      clickX = (x.getSceneX / tileSize).toInt
      clickY = (x.getSceneY / tileSize).toInt
    })


    val scene = new Scene(root, fullSize, fullSize)
    scene.setOnKeyReleased(k => {
      val c = k.getText.head
      if (clickX >= 0 && clickY >= 0) {
        board = board.update(clickX, clickY, c)
        board.tiles.zipWithIndex.foreach { case (line, i) =>
          line.zipWithIndex.foreach { case (tile, j) =>
            labels(i)(j).setText(tile.toString)
          }
        }
      }
    })

    (tileSize until fullSize by tileSize) foreach {
      x =>

        val l1 = new Line(x, 0, x, fullSize)
        val l2 = new Line(0, x, fullSize, x)

        if (x % 90 == 0) {
          l1.setStrokeWidth(2)
          l2.setStrokeWidth(2)
        }

        root.getChildren.addAll(l1, l2)
    }

    labels.foreach(x => x.foreach(y => root.getChildren.add(y)))

    primaryStage.setTitle("Sudoku Solver")
    primaryStage.setScene(scene)
    primaryStage.show()
  }

}