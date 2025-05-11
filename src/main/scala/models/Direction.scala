package models

enum Direction(name: String) {
  case Up extends Direction("Up")
  case Down extends Direction("Down")
  case Right extends Direction("Right")
  case Left extends Direction("Left")

  def apply(name: String): Direction = name match {
    case "Up"    => Up
    case "Down"  => Down
    case "Right" => Right
    case "Left"  => Left
    case _ => throw new IllegalArgumentException(s"Invalid direction: $name")
  }

  override def toString: String = name
}