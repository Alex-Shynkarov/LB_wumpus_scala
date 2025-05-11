package actors

import akka.actor.typed.Behavior
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import models._
import protocols._
import scala.util.Random

object SpeleologistActor {

  // Map of natural language messages for perceptions
  private val naturalLanguageObservations: Map[String, List[String]] = Map(
    "Breeze" -> List("It's windy in there.", "I feel some breeze."),
    "Stench" -> List(
      "Strong stench hits my nose.",
      "Awful odor turns me inside out."
    ),
    "Scream" -> List(
      "Cries of some creature echoing through the cave.",
      "Some animal yells in agony, my arrow must have hit it."
    ),
    "Bump" -> List(
      "I hit the wall with my forehead.",
      "I rushed forward, but there was only the wall."
    ),
    "Glitter" -> List(
      "I see something shiny on the floor.",
      "Light of the torch reflects from the pile of shining artifacts."
    )
  )

  // Convert raw perceptions to natural language observations
  private def compileNaturalObservation(perception: Perception): String = {
    val random = new Random()
    val observations = List.newBuilder[String]

    if (perception.breeze) {
      val options = naturalLanguageObservations("Breeze")
      observations.addOne(options(random.nextInt(options.size)))
    }

    if (perception.stench) {
      val options = naturalLanguageObservations("Stench")
      observations.addOne(options(random.nextInt(options.size)))
    }

    if (perception.bump) {
      val options = naturalLanguageObservations("Bump")
      observations.addOne(options(random.nextInt(options.size)))
    }

    if (perception.scream) {
      val options = naturalLanguageObservations("Scream")
      observations.addOne(options(random.nextInt(options.size)))
    }

    if (perception.glitter) {
      val options = naturalLanguageObservations("Glitter")
      observations.addOne(options(random.nextInt(options.size)))
    }

    observations.result().mkString("\n")
  }

  def apply(
             environment: ActorRef[EnvironmentProtocol.Command],
             navigator: ActorRef[NavigatorProtocol.Command]
           ): Behavior[SpeleologistProtocol.Command] = {
    Behaviors.setup { context =>
      context.log.info("Speleologist actor started")

      Behaviors.receiveMessage {
        case SpeleologistProtocol.StartExploration =>
          context.log.info("[Speleologist] Starting exploration...")
          environment ! EnvironmentProtocol.GetPerception(context.self)

          exploring(environment, navigator, context)

        case _ =>
          context.log.warn(
            "[Speleologist] Received unexpected message before exploration started"
          )
          Behaviors.same
      }
    }
  }

  private def exploring(
                         environment: ActorRef[EnvironmentProtocol.Command],
                         navigator: ActorRef[NavigatorProtocol.Command],
                         context: ActorContext[SpeleologistProtocol.Command]
                       ): Behavior[SpeleologistProtocol.Command] = {
    Behaviors.receiveMessage {
      case SpeleologistProtocol.PerceptionResponse(perception) =>
        context.log.debug(s"[Speleologist] Received perceptions: $perception")

        // Convert perception to natural language
        val naturalObservation = compileNaturalObservation(perception)
        context.log.info(s"[Speleologist] Observations:\n$naturalObservation")

        // Ask navigator for advice
        navigator ! NavigatorProtocol.RequestAdvice(
          naturalObservation,
          context.self
        )

        Behaviors.same

      case SpeleologistProtocol.NavigatorAdvice(action) =>
        context.log.info(s"[Speleologist] Navigator advised action: $action")

        // Perform the recommended action
        environment ! EnvironmentProtocol.PerformAction(action, context.self)

        // Special handling for Climb action - might end the game
        if (action == Action.Climb) {
          terminationHandling(environment, navigator, context)
        } else {
          Behaviors.same
        }

      case SpeleologistProtocol.ActionResult(success) =>
        if (!success) {
          // Failure = death
          context.log.error(
            "[Speleologist] Action failed! The speleologist has died."
          )
          context.self ! SpeleologistProtocol.GameOver
          Behaviors.same
        } else {
          // Request new perception
          environment ! EnvironmentProtocol.GetPerception(context.self)
          Behaviors.same
        }

      case SpeleologistProtocol.GameOver =>
        context.log.info("[Speleologist] Game over!")
        Behaviors.stopped

      case _ =>
        context.log.warn(
          "[Speleologist] Received unexpected message during exploration"
        )
        Behaviors.same
    }
  }

  private def terminationHandling(
                                   environment: ActorRef[EnvironmentProtocol.Command],
                                   navigator: ActorRef[NavigatorProtocol.Command],
                                   context: ActorContext[SpeleologistProtocol.Command]
                                 ): Behavior[SpeleologistProtocol.Command] = {
    Behaviors.receiveMessage {
      case SpeleologistProtocol.ActionResult(success) =>
        if (success) {
          context.log.info(
            "[Speleologist] Successfully climbed out with the gold!"
          )
        } else {
          context.log.error(
            "[Speleologist] Failed to climb out! No gold was collected."
          )
        }
        context.self ! SpeleologistProtocol.GameOver
        Behaviors.same

      case SpeleologistProtocol.GameOver =>
        context.log.info("[Speleologist] Game over!")
        Behaviors.stopped

      case _ =>
        context.log.warn(
          "[Speleologist] Received unexpected message during termination"
        )
        Behaviors.same
    }
  }
}