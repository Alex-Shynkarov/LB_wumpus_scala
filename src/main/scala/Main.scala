import akka.actor.typed.{ActorSystem, Behavior, Terminated}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.CoordinatedShutdown
import actors._
import protocols._

object Main extends App:
  val rootBehavior = Behaviors.setup[Nothing] { context =>
    // Spawn the environment actor
    val environmentActor = context.spawn(EnvironmentActor(), "EnvironmentActor")

    // Spawn the navigator actor
    val navigatorActor = context.spawn(NavigatorActor(), "NavigatorActor")

    // Spawn the speleologist actor with references to environment and navigator
    val speleologistActor = context.spawn(
      SpeleologistActor(environmentActor, navigatorActor),
      "SpeleologistActor"
    )

    // Watch the speleologist to detect termination
    context.watch(speleologistActor)

    // Start the exploration
    speleologistActor ! SpeleologistProtocol.StartExploration

    // Handle termination of the speleologist
    Behaviors.receiveSignal { case (_, Terminated(_)) =>
      println("Wumpus World finished. Performing coordinated shutdown...")
      CoordinatedShutdown(context.system).run(CoordinatedShutdown.JvmExitReason)
      Behaviors.stopped
    }
  }

  // Create the actor system with the root behavior
  val system = ActorSystem[Nothing](rootBehavior, "WumpusWorld")