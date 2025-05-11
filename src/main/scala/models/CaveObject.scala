package models

enum CaveObject(name: String) {
  case Empty extends CaveObject("   ")
  case Pit extends CaveObject(" P ")
  case LiveWumpus extends CaveObject(" W ")
  case DeadWumpus extends CaveObject(" X ")
  case Gold extends CaveObject(" G ")
}