package actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import models._
import protocols._

object EnvironmentActor {
  private case class EnvironmentState(
                                       cave: Array[Array[CaveObject]],
                                       speleologistState: AgentState = AgentState()
                                       // speleologistState: Option[AgentState]
                                     )

  private def initCave(): Array[Array[CaveObject]] = {
    val cave = Array.fill(4, 4)(CaveObject.Empty)

    // Place pits, wumpus and gold
    cave(2)(1) = CaveObject.LiveWumpus // (3,1)
    cave(3)(1) = CaveObject.Gold // (3,2)
    cave(0)(1) = CaveObject.Pit // (1,3)
    cave(2)(1) = CaveObject.Pit // (3,3)
    cave(3)(2) = CaveObject.Pit // (4,4)
    cave
  }

  // Convert from 1-indexed Position to 0-indexed array coordinates
  private def positionToArrayCoords(pos: Position): (Int, Int) = {
    // Cave is represented as (0,0) in bottom left, but arrays are indexed from top
    // So convert y to the inverse
    val x = pos.x - 1
    val y = 4 - pos.y
    (y, x)
  }

  private def checkForBreeze(
                              cave: Array[Array[CaveObject]],
                              position: Position
                            ): Boolean = {
    Utils.adjacentPositions(position).exists { adjPos =>
      val (y, x) = positionToArrayCoords(adjPos)
      y >= 0 && y < 4 && x >= 0 && x < 4 && cave(y)(x) == CaveObject.Pit
    }
  }

  private def checkForStench(
                              cave: Array[Array[CaveObject]],
                              position: Position
                            ): Boolean = {
    Utils.adjacentPositions(position).exists { adjPos =>
      val (y, x) = positionToArrayCoords(adjPos)
      y >= 0 && y < 4 && x >= 0 && x < 4 &&
        (cave(y)(x) == CaveObject.LiveWumpus || cave(y)(
          x
        ) == CaveObject.DeadWumpus)
    }
  }

  // Check for glitter perception
  private def checkForGlitter(
                               cave: Array[Array[CaveObject]],
                               position: Position
                             ): Boolean = {
    val (y, x) = positionToArrayCoords(position)
    y >= 0 && y < 4 && x >= 0 && x < 4 && cave(y)(x) == CaveObject.Gold
  }

  // Print the cave with the speleologist position
  private def printCave(
                         cave: Array[Array[CaveObject]],
                         agentState: AgentState
                       ): Unit = {
    println("Current cave state:")

    for (y <- 0 until 4) {
      val line = new StringBuilder()
      for (x <- 0 until 4) {
        // Convert array coordinates back to Position coordinates
        val position = Position(x + 1, 4 - y)

        if (position == agentState.position) {
          line.append("| A |")
        } else {
          cave(3 - y)(x) match {
            case CaveObject.Empty      => line.append("|   |")
            case CaveObject.Gold       => line.append("| G |")
            case CaveObject.Pit        => line.append("| P |")
            case CaveObject.LiveWumpus => line.append("| W |")
            case CaveObject.DeadWumpus => line.append("| w |")
          }
        }
      }
      println(line.toString)
    }
  }

  // Get perception at current position
  private def perceive(state: EnvironmentState): AgentState = {
    val agentState = state.speleologistState

    val breeze = checkForBreeze(state.cave, agentState.position)
    val stench = checkForStench(state.cave, agentState.position)
    val glitter = checkForGlitter(state.cave, agentState.position)

    println(s"Breeze: $breeze, Stench: $stench, Glitter: $glitter")
    println(s"Agent position: ${agentState.position}")

    agentState.copy(breeze = breeze, stench = stench, glitter = glitter)
  }

  // Process agent actions
  private def processAction(
                             state: EnvironmentState,
                             action: Action
                           ): (EnvironmentState, Boolean) = {
    val agentState = state.speleologistState

    val newTick = agentState.tick + 1

    // Create a mutable copy of the cave to modify
    val newCave = state.cave.map(_.clone())

    // Process the action and get updated state
    val (newAgentState, success) = action match {
      case Action.TurnLeft =>
        val newDirection = Utils.turnLeft(agentState.direction)
        (agentState.copy(direction = newDirection, tick = newTick), true)

      case Action.TurnRight =>
        val newDirection = Utils.turnRight(agentState.direction)
        (agentState.copy(direction = newDirection, tick = newTick), true)

      case Action.Forward =>
        val delta = Utils.directionToDelta(agentState.direction)
        val targetPosition = Position(
          agentState.position.x + delta.x,
          agentState.position.y + delta.y
        )

        if (Utils.isOutOfBounds(targetPosition)) {
          // Bump into wall
          (agentState.copy(bump = true, tick = newTick), true)
        } else {
          val (y, x) = positionToArrayCoords(targetPosition)
          newCave(y)(x) match {
            case CaveObject.Pit | CaveObject.LiveWumpus =>
              // Die by falling into a pit or being eaten by wumpus
              (agentState.copy(tick = newTick), false)
            case _ =>
              // Move to new position
              (agentState.copy(position = targetPosition, tick = newTick), true)
          }
        }

      case Action.Shoot =>
        var arrowPosition = agentState.position
        val delta = Utils.directionToDelta(agentState.direction)
        var hitWumpus = false

        // Trace the arrow's path
        while (!Utils.isOutOfBounds(arrowPosition) && !hitWumpus) {
          arrowPosition = Position(
            arrowPosition.x + delta.x,
            arrowPosition.y + delta.y
          )

          if (!Utils.isOutOfBounds(arrowPosition)) {
            val (y, x) = positionToArrayCoords(arrowPosition)
            if (newCave(y)(x) == CaveObject.LiveWumpus) {
              hitWumpus = true
              newCave(y)(x) = CaveObject.DeadWumpus
            }
          }
        }

        (agentState.copy(scream = hitWumpus, tick = newTick), true)

      case Action.Grab =>
        val (y, x) = positionToArrayCoords(agentState.position)
        val gotGold = newCave(y)(x) == CaveObject.Gold

        if (gotGold) {
          newCave(y)(x) = CaveObject.Empty
          (agentState.copy(hasGold = true, tick = newTick), true)
        } else {
          (agentState.copy(tick = newTick), true)
        }

      case Action.Climb =>
        if (agentState.position == Position(1, 1) && agentState.hasGold) {
          (agentState.copy(tick = newTick), true)
        } else {
          (agentState.copy(tick = newTick), false)
        }
    }

    val updatedAgentState = newAgentState.timeStep
    (EnvironmentState(newCave, updatedAgentState), success)
  }

  def apply(): Behavior[EnvironmentProtocol.Command] = {
    Behaviors.setup { context =>
      context.log.info("Environment actor started")
      val initialState = EnvironmentState(initCave())
      handleCommands(initialState, context)
    }
  }

  private def handleCommands(
                              state: EnvironmentState,
                              context: ActorContext[EnvironmentProtocol.Command]
                            ): Behavior[EnvironmentProtocol.Command] = {
    Behaviors.receiveMessage {
      // Perception request
      case EnvironmentProtocol.GetPerception(replyTo) =>
        val agentState = state.speleologistState
        val updatedAgentState = perceive(state)

        val newState = state.copy(speleologistState = updatedAgentState)

        // Print the cave
        if (updatedAgentState != agentState) {
          printCave(state.cave, updatedAgentState)
        }

        // Create the perception message to send back
        val perception = Perception(
          updatedAgentState.breeze,
          updatedAgentState.stench,
          updatedAgentState.bump,
          updatedAgentState.scream,
          updatedAgentState.glitter,
          updatedAgentState.tick
        )

        // Send perception to the speleologist
        context.log.debug(s"Sending perception: $perception")
        replyTo ! SpeleologistProtocol.PerceptionResponse(perception)

        handleCommands(newState, context)

      // Action performance request
      case EnvironmentProtocol.PerformAction(action, replyTo) =>
        context.log.debug(s"Received action request: $action")

        val (newState, success) = processAction(state, action)

        printCave(newState.cave, newState.speleologistState)
        // if (newState.speleologistState.isDefined) {
        // }

        // Send result back to the speleologist
        replyTo ! SpeleologistProtocol.ActionResult(success)

        handleCommands(newState, context)
    }
  }
}
