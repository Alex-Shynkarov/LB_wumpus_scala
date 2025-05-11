package protocols

import akka.actor.typed.ActorRef
import models._

object EnvironmentProtocol {
  sealed trait Command

  case class GetPerception(replyTo: ActorRef[SpeleologistProtocol.Command])
    extends Command

  case class PerformAction(
                            action: Action,
                            replyTo: ActorRef[SpeleologistProtocol.Command]
                          ) extends Command
}