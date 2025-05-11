package protocols

import akka.actor.typed.ActorRef
import models._

object SpeleologistProtocol {
  sealed trait Command

  // Response from Environment containing perception
  case class PerceptionResponse(perception: Perception) extends Command

  // Response from Environment after action performance
  case class ActionResult(success: Boolean) extends Command

  // Response from Navigator with recommended action
  case class NavigatorAdvice(action: Action) extends Command

  // Command to start the game
  case object StartExploration extends Command

  // Command to notify of game termination
  case object GameOver extends Command
}