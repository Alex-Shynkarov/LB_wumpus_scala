package models

case class Perception(
                       breeze: Boolean = false,
                       stench: Boolean = false,
                       bump: Boolean = false,
                       scream: Boolean = false,
                       glitter: Boolean = false,
                       tick: Int = 0
                     ) {
  override def toString: String =
    s"($breeze, $stench, $bump, $scream, $glitter, $tick)"
}