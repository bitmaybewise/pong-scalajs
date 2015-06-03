package pong

import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom._
import dom.html.Div
import scala.scalajs.js.annotation.JSExport
import scala.collection.mutable.Map

object Status extends Enumeration {
  val Stoped, Running, GameOver = Value
}

object Key extends Enumeration {
  val Left  = 37
  val Right = 39
}

@JSExport
object game {
  import Status._

  private var pressedKey: Map[Int, Boolean] = Map(Key.Left -> false, Key.Right -> false)
  private var status = Stoped
  private var score  = 0

  private object ball {
    val speed = 5
    var x = 135
    var y = 100
    var directionX = -1
    var directionY = -1
  }

  @JSExport
  def load(playgroundHTML: Div, racketHTML: Div, ballHTML: Div, scoreHTML: Div, startHTML: Div, gameOverHTML: Div): Unit = {
    val executeLoop = () => { loop(playgroundHTML, racketHTML, ballHTML, scoreHTML, gameOverHTML) }
    dom.setInterval(executeLoop, 16)
    val markAsPressed = (evt: KeyboardEvent) => pressedKey(evt.keyCode) = true
    document.addEventListener("keydown", markAsPressed)
    val markAsNotPressed = (evt: KeyboardEvent) => pressedKey(evt.keyCode) = false
    document.addEventListener("keyup", markAsNotPressed)
    var startGame = (_: KeyboardEvent) => {
      if (isStoped) {
        status = Running
        startHTML.style.display = "none"
      }
    }
    document.addEventListener("keydown", startGame)
  }

  private def isStoped: Boolean  = status == Stoped

  private def isRunning: Boolean = status == Running
  
  private def loop(playgroundHTML: Div, racketHTML: Div, ballHTML: Div, scoreHTML: Div, gameOverHTML: Div) {
    if (isRunning) {
      val newDirX  = moveBallDirectionX(playgroundHTML)
      val newDirY  = moveBallDirectionY(playgroundHTML)
      val newPosX  = moveBallPosition(newDirX)
      val newPosY  = moveBallPosition(newDirY)
      changeBallPosition(newDirX, newPosX, newDirY, newPosY)
      drawBall(ballHTML)
      val pixelPos = moveRacket(racketHTML)
      drawRacket(racketHTML, pixelPos)
      val hit      = counter(racketHTML, ballHTML)
      val newScore = computeScore(hit, score)
      changeDirectionY(hit)
      changeScore(newScore)
      drawScore(scoreHTML)
      val isOver = isGameOver(racketHTML, ballHTML)
      endGame(isOver)
      drawEndGame(gameOverHTML, isOver)
    }
  }

  private def nextPosition(currentPosition: Int, speed: Int, direction: Int): Int = currentPosition + speed * direction

  private def moveBallDirectionX(playgroundHTML: Div): Int = {
    val width      = playgroundHTML.offsetWidth
    var directionX = ball.directionX
    var positionX  = nextPosition(ball.x, ball.speed, ball.directionX)
    if (positionX > width) directionX = -1
    if (positionX < 0) directionX = 1
    directionX
  }

  private def moveBallDirectionY(playgroundHTML: Div): Int = {
    val height     = playgroundHTML.offsetHeight
    var directionY = ball.directionY
    val positionY  = nextPosition(ball.y, ball.speed, ball.directionY)
    if (positionY > height) directionY = -1
    if (positionY < 0) directionY = 1
    directionY
  }

  private def moveBallPosition(direction: Int): Int = ball.speed * direction

  private def changeBallPosition(dirX: Int, posX: Int, dirY: Int, posY: Int) {
    ball.directionX = dirX
    ball.directionY = dirY
    ball.x += posX
    ball.y += posY
  }

  private def drawBall(ballHTML: Div) {
    ballHTML.style.left = ball.x + "px"
    ballHTML.style.top  = ball.y + "px"
  }

  private def moveRacket(racketHTML: Div): Int = {
    val left: Int = racketHTML.offsetLeft.toInt
    if (pressedKey(Key.Left)) left - 5
    else if (pressedKey(Key.Right)) left + 5
    else left
  }

  private def drawRacket(racketHTML: Div, pixelPos: Int) {
    racketHTML.style.left = pixelPos + "px"
  }

  private def racketPositionY(racketHTML: Div, ballHTML: Div): Int = {
    val ballSize = ballHTML.offsetHeight.toInt
    racketHTML.offsetTop.toInt - ballSize / 2 // subtracting size of ball for doesn't pass through racket
  }

  private def isRacketHit(racketHTML: Div, ballHTML: Div): Boolean = {
    val racketBorderLeft  = racketHTML.offsetLeft.toInt
    val racketBorderRight = racketBorderLeft + racketHTML.offsetWidth.toInt
    val posX              = nextPosition(ball.x, ball.speed, ball.directionX)
    val posY              = nextPosition(ball.y, ball.speed, ball.directionY)
    val racketPosY        = racketPositionY(racketHTML, ballHTML)
    posX >= racketBorderLeft && posX <= racketBorderRight && posY >= racketPosY
  }

  private def counter(racketHTML: Div, ballHTML: Div) = isRacketHit(racketHTML, ballHTML)

  private def computeScore(hit: Boolean, score: Int): Int = if (hit) score + 1 else score

  private def changeDirectionY(hit: Boolean) = if (hit) ball.directionY = -1

  private def changeScore(newScore: Int) = score = newScore

  private def drawScore(scoreHTML: Div) = scoreHTML.innerHTML = score.toString

  private def isGameOver(racketHTML: Div, ballHTML: Div): Boolean = {
    val bottomPos  = racketHTML.offsetHeight.toInt
    val posY       = nextPosition(ball.y, ball.speed, ball.directionY) - bottomPos
    val racketPosY = racketPositionY(racketHTML, ballHTML)
    posY > racketPosY;
  }

  private def endGame(isOver: Boolean) = if (isOver) status = GameOver

  private def drawEndGame(gameOverHTML: Div, isOver: Boolean) = if (isOver) gameOverHTML.style.display = "block"
}
