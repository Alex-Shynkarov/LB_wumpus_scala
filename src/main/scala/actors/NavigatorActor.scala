 package actors

 import akka.actor.typed.Behavior
 import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
 import models._
 import protocols._
 import scala.collection.immutable.{List, Map, Set}
 import scala.util.Random

 object NavigatorActor {
   // KnowledgeBase tracks what the navigator has learned about the cave
   private case class KnowledgeBase(
       observations: List[NavigatorProtocol.Observation] = List.empty
   ) {
     def getTimeStep: Int = observations.size

     def breeze(pos: Position, t: Int): Boolean =
       observations.take(t).exists(o => o.position == pos && o.breeze)

     def noBreeze(pos: Position, t: Int): Boolean =
       observations.take(t).exists(o => o.position == pos && !o.breeze)

     def pit(pos: Position, t: Int): Boolean = {
       val adjacentHaveBreeze =
         Utils.adjacentPositions(pos).forall(p => breeze(p, t))

       val adjacentBreezeMustBeFromHere =
         Utils.adjacentPositions(pos).exists { p =>
           breeze(p, t) && Utils
             .adjacentPositions(p)
             .filterNot(_ == pos)
             .forall(adj => noPit(adj, t))
         }

       adjacentHaveBreeze || adjacentBreezeMustBeFromHere
     }

     def noPit(pos: Position, t: Int): Boolean =
       Utils.adjacentPositions(pos).exists(p => noBreeze(p, t))

     def stench(pos: Position, t: Int): Boolean =
       observations.take(t).exists(o => o.position == pos && o.stench)

     def noStench(pos: Position, t: Int): Boolean =
       observations.take(t).exists(o => o.position == pos && !o.stench)

     def wumpus(pos: Position, t: Int): Boolean = {
       val noScream = !observations.take(t).exists(_.scream)

       val adjacentHaveStench =
         Utils.adjacentPositions(pos).forall(p => stench(p, t))

       val adjacentStenchMustBeFromHere =
         Utils.adjacentPositions(pos).exists { p =>
           stench(p, t) && Utils
             .adjacentPositions(p)
             .filterNot(_ == pos)
             .forall(adj => noWumpus(adj, t))
         }

       noScream && (adjacentHaveStench || adjacentStenchMustBeFromHere)
     }

     def noWumpus(pos: Position, t: Int): Boolean =
       observations.take(t).exists(_.scream) || Utils
         .adjacentPositions(pos)
         .exists(p => noStench(p, t))

     def noWumpus(pos: Position): Boolean = noWumpus(pos, getTimeStep)

     def visited(pos: Position, t: Int): Boolean =
       observations.take(t).exists(o => o.position == pos)

     def unvisited(pos: Position, t: Int): Boolean = !visited(pos, t)

     def unvisited(pos: Position): Boolean = unvisited(pos, getTimeStep)

     def safe(pos: Position, t: Int): Boolean = noWumpus(pos, t) && noPit(pos, t)

     def safe(pos: Position): Boolean = safe(pos, getTimeStep)

     def unsafe(pos: Position, t: Int): Boolean = wumpus(pos, t) || pit(pos, t)

     def unsafe(pos: Position): Boolean = unsafe(pos, getTimeStep)

     def observe(observation: NavigatorProtocol.Observation): KnowledgeBase =
       KnowledgeBase(observations :+ observation)

     def currentPosition: Position =
       if (observations.isEmpty) Position(1, 1)
       else observations.last.position

     def currentDirection: Direction =
       if (observations.isEmpty) Direction.Right
       else observations.last.direction
   }

   // Natural language perception keywords
   private val naturalLanguageKeywords: Map[String, Set[String]] = Map(
     "Breeze" -> Set("windy", "breeze"),
     "Stench" -> Set("stench", "odor"),
     "Scream" -> Set("cries", "yells"),
     "Bump" -> Set("wall"),
     "Glitter" -> Set("shiny", "shining")
   )

   // Check if the message contains keywords for a perception
   private def containsKeywords(
       message: String,
       keywords: Set[String]
   ): Boolean =
     keywords.exists(message.toLowerCase.contains)

   // Parse natural language observations
   private def parseObservation(
       message: String,
       position: Position,
       direction: Direction
   ): NavigatorProtocol.Observation = {
     val breeze = containsKeywords(message, naturalLanguageKeywords("Breeze"))
     val stench = containsKeywords(message, naturalLanguageKeywords("Stench"))
     val scream = containsKeywords(message, naturalLanguageKeywords("Scream"))
     val glitter = containsKeywords(message, naturalLanguageKeywords("Glitter"))

     NavigatorProtocol.Observation(
       position,
       direction,
       breeze,
       stench,
       scream,
       glitter
     )
   }

   // Search for a path from start to any target position through allowed positions
   private def search(
       start: Position,
       targets: Set[Position],
       allowed: Set[Position],
       acc: List[Position] = List.empty
   ): List[Position] = {
     if (targets.isEmpty) {
       List.empty
     } else if (targets.contains(start)) {
       acc :+ start
     } else {
       // Find next positions to explore, sorted by Manhattan distance to nearest target
       val nextPositions = Utils
         .adjacentPositions(start)
         .filter(p => allowed.contains(p) || targets.contains(p))
         .sortBy { pos =>
           targets
             .map(target =>
               Math.abs(target.x - pos.x) + Math.abs(target.y - pos.y)
             )
             .min
         }

       if (nextPositions.isEmpty) {
         if (acc.isEmpty) {
           List.empty
         } else {
           val newAllowed = allowed - acc.last
           search(acc.last, targets, newAllowed, acc.dropRight(1))
         }
       } else {
         val nextPos = nextPositions.head
         val newAllowed = allowed - nextPos
         val newAcc = acc :+ start
         search(nextPos, targets, newAllowed, newAcc)
       }
     }
   }

   // Convert a route to a list of actions
   private def routeToActions(
       startDirection: Direction,
       route: List[Position]
   ): List[Action] = {
     if (route.size < 2) {
       List.empty
     } else {
       var actions = List.empty[Action]
       var currentDirection = startDirection

       for (i <- 0 until route.size - 1) {
         val current = route(i)
         val next = route(i + 1)
         val delta = Position(next.x - current.x, next.y - current.y)

         val nextDirection = Utils.deltaToDirection(delta)
         actions = actions ++ Utils.turn(currentDirection, nextDirection)
         actions = actions :+ Action.Forward
         currentDirection = nextDirection
       }

       actions
     }
   }

   // Plan a route from start to any target through allowed positions
   private def planRoute(
       start: (Position, Direction),
       targets: Set[Position],
       allowed: Set[Position]
   ): List[Action] = {
     val newAllowed = allowed - start._1
     val route = search(start._1, targets, newAllowed)
     routeToActions(start._2, route)
   }

   // Plan a shot from start to any position adjacent to a target
   private def planShot(
       start: (Position, Direction),
       possibleTargets: Set[Position],
       allowed: Set[Position]
   ): List[Action] = {
     val newAllowed = allowed - start._1

     // Find positions from which we can shoot a target
     val shootingPositions = Utils.allPositions
       .filter(allowed.contains)
       .filter { pos =>
         possibleTargets.exists { target =>
           // Must be in same row or column to shoot
           pos.x == target.x || pos.y == target.y
         }
       }
       .toSet

     if (shootingPositions.isEmpty) {
       return List.empty
     }

     // Find route to a shooting position
     val route = search(start._1, shootingPositions, newAllowed)

     if (route.isEmpty) {
       return List.empty
     }

     // Get actions to reach the shooting position
     val actions = routeToActions(start._2, route)

     // Determine last position and direction
     val lastPosition = route.last
     val lastDirection = if (route.size < 2) {
       start._2
     } else {
       val secondLast = route(route.size - 2)
       val delta =
         Position(lastPosition.x - secondLast.x, lastPosition.y - secondLast.y)
       Utils.deltaToDirection(delta)
     }

     // Find a target that can be shot from the last position
     val targetPosition = possibleTargets
       .find { target =>
         target.x == lastPosition.x || target.y == lastPosition.y
       }
       .getOrElse(throw new RuntimeException("[Navigator] No targets available"))

     // Determine direction to face for the shot
     val targetDirection =
       if (
         lastPosition.x == targetPosition.x && lastPosition.y > targetPosition.y
       ) {
         Direction.Down
       } else if (
         lastPosition.x == targetPosition.x && lastPosition.y < targetPosition.y
       ) {
         Direction.Up
       } else if (
         lastPosition.y == targetPosition.y && lastPosition.x < targetPosition.x
       ) {
         Direction.Right
       } else {
         Direction.Left
       }

     // Add turning actions and shoot
     val turnActions = Utils.turn(lastDirection, targetDirection)
     actions ++ turnActions :+ Action.Shoot
   }

   def apply(): Behavior[NavigatorProtocol.Command] = {
     Behaviors.setup { context =>
       context.log.info("Navigator actor started")
       handleCommands(KnowledgeBase(), List.empty, true, context)
     }
   }

   private def handleCommands(
       kb: KnowledgeBase,
       plan: List[Action],
       hasArrow: Boolean,
       context: ActorContext[NavigatorProtocol.Command]
   ): Behavior[NavigatorProtocol.Command] = {
     Behaviors.receiveMessage {
       case NavigatorProtocol.RequestAdvice(observation, replyTo) =>
         context.log.debug(s"Processing observation: $observation")

         val parsedObservation = parseObservation(
           observation,
           kb.currentPosition,
           kb.currentDirection
         )

         val newKb = kb.observe(parsedObservation)

         val (action, newPlan, newHasArrow) =
           selectAction(newKb, plan, hasArrow, context)

         context.log.debug(s"Selected action: $action, remaining plan: $newPlan")

         replyTo ! SpeleologistProtocol.NavigatorAdvice(action)

         handleCommands(newKb, newPlan, newHasArrow, context)

       case NavigatorProtocol.GetState(replyTo) =>
         replyTo ! NavigatorProtocol.KnowledgeBaseState(
           kb.observations,
           plan,
           hasArrow
         )
         Behaviors.same
     }
   }

   private def selectAction(
       kb: KnowledgeBase,
       currentPlan: List[Action],
       hasArrow: Boolean,
       context: ActorContext[NavigatorProtocol.Command]
   ): (Action, List[Action], Boolean) = {
     val goldDetected = kb.observations.nonEmpty && kb.observations.last.glitter

     if (goldDetected && !currentPlan.contains(Action.Grab)) {
       context.log.info("[Navigator] Gold found!")
       val route = planRoute(
         (kb.currentPosition, kb.currentDirection),
         Set(Position(1, 1)),
         Utils.allPositions.filter(kb.safe).toSet
       )

       val newPlan = Action.Grab :: (route ++ List(Action.Climb))
       return (Action.Grab, newPlan.tail, hasArrow)
     }

     // If we have a plan, execute it
     if (currentPlan.nonEmpty) {
       val action = currentPlan.head
       val newHasArrow = if (action == Action.Shoot) false else hasArrow
       return (action, currentPlan.tail, newHasArrow)
     }

     // Safe positions
     val safePositions = Utils.allPositions.filter(kb.safe).toSet

     // Unvisited positions
     val unvisitedPositions = Utils.allPositions.filter(kb.unvisited).toSet

     // Find safe and unvisited positions to explore
     val targets = safePositions.intersect(unvisitedPositions)

     // Plan route to safe unvisited positions
     if (targets.nonEmpty) {
       context.log.info(
         "[Navigator] Planning route to safe unvisited position..."
       )
       val route = planRoute(
         (kb.currentPosition, kb.currentDirection),
         targets,
         safePositions
       )

       if (route.nonEmpty) {
         return (route.head, route.tail, hasArrow)
       }
     }

     // Plan a shot if we have an arrow and there are possible wumpus positions
     if (hasArrow) {
       context.log.info("[Navigator] Planning shot...")
       val possibleWumpus = Utils.allPositions.filterNot(kb.noWumpus).toSet

       if (possibleWumpus.nonEmpty) {
         val shotPlan = planShot(
           (kb.currentPosition, kb.currentDirection),
           possibleWumpus,
           safePositions
         )

         if (shotPlan.nonEmpty) {
           return (shotPlan.head, shotPlan.tail, hasArrow)
         }
       }
     }

     // If we can't find a safe path, try to explore uncertain positions
     val notUnsafePositions = Utils.allPositions.filterNot(kb.unsafe).toSet
     val uncertainTargets = notUnsafePositions.intersect(unvisitedPositions)

     if (uncertainTargets.nonEmpty) {
       context.log.info("[Navigator] Exploring uncertain area...")
       val route = planRoute(
         (kb.currentPosition, kb.currentDirection),
         uncertainTargets,
         safePositions
       )

       if (route.nonEmpty) {
         return (route.head, route.tail, hasArrow)
       }
     }

     // If all else fails, go back to start and climb out
     context.log.info("[Navigator] No targets available. Returning to start...")
     val routeToStart = planRoute(
       (kb.currentPosition, kb.currentDirection),
       Set(Position(1, 1)),
       safePositions
     )

     val exitPlan = if (routeToStart.nonEmpty) {
       routeToStart :+ Action.Climb
     } else {
       List(Action.Climb)
     }

     if (exitPlan.nonEmpty) {
       (exitPlan.head, exitPlan.tail, hasArrow)
     } else {
       // Last resort
       (Action.Climb, List.empty, hasArrow)
     }
   }

 }