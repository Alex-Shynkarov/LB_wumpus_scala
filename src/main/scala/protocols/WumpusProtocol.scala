package protocols

import akka.actor.typed.ActorRef
import models._
import protocols._

object WumpusProtocol {
  sealed trait Command

  case class CreateEnvironment(replyTo: ActorRef[EnvironmentCreated])
    extends Command
  case class EnvironmentCreated(
                                 environmentRef: ActorRef[EnvironmentProtocol.Command]
                               )

  case class CreateNavigator(replyTo: ActorRef[NavigatorCreated])
    extends Command
  case class NavigatorCreated(navigatorRef: ActorRef[NavigatorProtocol.Command])

  case class CreateSpeleologist(
                                 environmentRef: ActorRef[EnvironmentProtocol.Command],
                                 navigatorRef: ActorRef[NavigatorProtocol.Command],
                                 replyTo: ActorRef[SpeleologistCreated]
                               ) extends Command
  case class SpeleologistCreated(
                                  speleologistRef: ActorRef[SpeleologistProtocol.Command]
                                )

  case class StartGame(speleologistRef: ActorRef[SpeleologistProtocol.Command])
    extends Command

  case object Shutdown extends Command
}