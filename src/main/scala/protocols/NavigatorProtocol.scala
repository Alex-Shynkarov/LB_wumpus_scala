package protocols

import akka.actor.typed.ActorRef
import models._
import protocols._

object NavigatorProtocol {
  sealed trait Command

  case class RequestAdvice(
                            naturalLanguageObservation: String,
                            replyTo: ActorRef[SpeleologistProtocol.Command]
                          ) extends Command

  case class GetState(replyTo: ActorRef[KnowledgeBaseState]) extends Command
  case class KnowledgeBaseState(
                                 observations: List[Observation],
                                 plan: List[Action],
                                 hasArrow: Boolean
                               )

  case class Observation(
                          position: Position,
                          direction: Direction,
                          breeze: Boolean,
                          stench: Boolean,
                          scream: Boolean,
                          glitter: Boolean,
                          tick: Int = 0
                        )
}