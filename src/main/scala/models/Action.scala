package models

enum Action(val name: String) {
  case TurnLeft extends Action("TurnLeft")
  case TurnRight extends Action("TurnRight")
  case Forward extends Action("Forward")
  case Shoot extends Action("Shoot")
  case Grab extends Action("Grab")
  case Climb extends Action("Climb")
}