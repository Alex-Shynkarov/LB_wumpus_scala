package models

import models._

case class AgentState(
                       position: Position = Position(1, 1),
                       direction: Direction = Direction.Right,
                       breeze: Boolean = false,
                       stench: Boolean = false,
                       bump: Boolean = false,
                       scream: Boolean = false,
                       glitter: Boolean = false,
                       hasGold: Boolean = false,
                       tick: Int = 0
                     ) {
  def timeStep: AgentState = copy(
    breeze = false,
    stench = false,
    bump = false,
    scream = false,
    glitter = false,
    tick = tick + 1
  )
}