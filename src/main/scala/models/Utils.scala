package models

import models._

object Utils {
  def directionToDelta(direction: Direction): Position = direction match {
    case Direction.Up    => Position(0, 1)
    case Direction.Down  => Position(0, -1)
    case Direction.Left  => Position(-1, 0)
    case Direction.Right => Position(1, 0)
  }

  def turnLeft(direction: Direction): Direction = direction match {
    case Direction.Up    => Direction.Left
    case Direction.Left  => Direction.Down
    case Direction.Down  => Direction.Right
    case Direction.Right => Direction.Up
  }

  def turnRight(direction: Direction): Direction = direction match {
    case Direction.Up    => Direction.Right
    case Direction.Right => Direction.Down
    case Direction.Down  => Direction.Left
    case Direction.Left  => Direction.Up
  }

  def isOutOfBounds(position: Position): Boolean =
    position.x < 1 || position.x > 4 || position.y < 1 || position.y > 4

  def adjacentPositions(position: Position): List[Position] = {
    val deltas = List(
      Position(0, 1), // up
      Position(0, -1), // down
      Position(-1, 0), // left
      Position(1, 0) // right
    )

    deltas
      .map(delta => Position(position.x + delta.x, position.y + delta.y))
      .filterNot(isOutOfBounds)
  }

  def deltaToDirection(delta: Position): Direction = {
    if (delta.x == 0 && delta.y > 0) Direction.Up
    else if (delta.x == 0 && delta.y < 0) Direction.Down
    else if (delta.x < 0 && delta.y == 0) Direction.Left
    else Direction.Right
  }

  val allPositions: List[Position] = {
    for {
      x <- (1 to 4).toList
      y <- (1 to 4).toList
    } yield Position(x, y)
  }

  def turn(from: Direction, to: Direction): List[Action] = {
    if (from == to) return List()

    var currentDir = from
    var leftTurns = 0
    while (currentDir != to && leftTurns < 4) {
      currentDir = turnLeft(currentDir)
      leftTurns += 1
    }

    currentDir = from
    var rightTurns = 0
    while (currentDir != to && rightTurns < 4) {
      currentDir = turnRight(currentDir)
      rightTurns += 1
    }

    if (leftTurns <= rightTurns && leftTurns < 4) {
      List.fill(leftTurns)(Action.TurnLeft)
    } else if (rightTurns < 4) {
      List.fill(rightTurns)(Action.TurnRight)
    } else {
      List()
    }
  }
}