package de.fhwedel.antscout

import antnet.AntMap
import osm.OsmMap
import routing.RoutingService
import akka.actor.{Props, Actor, FSM}
import net.liftweb.http.LiftSession

/**
 * Initialisiert die Anwendung mit Hilfe eines Zustands-Automaten.
 */
class AntScout extends Actor with FSM[AntScoutState, Unit] {

  import AntScout._

  // AntNodeSupervisor erzeugen
  context.actorOf(Props[AntNodeSupervisor].withDispatcher("ant-node-supervisor-dispatcher"),
    AntNodeSupervisor.ActorName)
  // Routing-Service erzeugen
  context.actorOf(Props[RoutingService], RoutingService.ActorName)

  // Start-Zustand
  startWith(Uninitialized, Unit)

  // Uninitialisiert
  when(Uninitialized) {
    // Initialisierung anstoÃŸen
    case Event(Initialize, _) =>
      // OsmMap initialisieren
      OsmMap(Settings.Map)
      // RoutingService-Initialisierung anstoÃŸen
      context.actorFor(RoutingService.ActorName) ! RoutingService.Initialize
      // Zustand wechseln
      goto(InitializingRoutingService)
  }

  // RoutingService-Initialisierung
  when(InitializingRoutingService) {
    // RoutingService initialisiert
    case Event(RoutingServiceInitialized, _) =>
      // Ant-Weg-Daten berechnen
      val antWayData = AntMap.prepare
      // Ant-Knoten berechnen
      AntMap.computeNodes(antWayData)
      // AntNodeSupervisor-Initialisierung anstoÃŸen
      context.actorFor(AntNodeSupervisor.ActorName) ! AntNodeSupervisor.Initialize(antWayData)
      // Zustand wechseln
      goto(InitializingAntNodeSupervisor)
  }

  // AntNodeSupervisor-Initialisierung
  when(InitializingAntNodeSupervisor) {
    // AntNodeSupervisor initialisiert
    case Event(AntNodeSupervisor.Initialized(antWayData), _) =>
      // Ant-Wege berechnen
      AntMap.computeAntWays(antWayData)
      // Ein- und ausgehende Wege berechnen
      AntMap.computeIncomingAndOutgoingWays()
      // Quellen und Ziele berechnen
      AntMap.computeSourcesAndDestinations()
      // Zusicherung, dass Knoten nicht leer sind
      assert(AntMap.nodes.size > 0, AntMap.nodes.size)
      // Initialisierung der Ant-Knoten anstoÃŸen
      context.actorFor(AntNodeSupervisor.ActorName) ! AntNodeSupervisor.InitializeNodes
      // In diesem Zustand bleiben
      stay()
  }

  // Unbehandelte Nachrichten
  whenUnhandled {
    // Lift-Session
    case Event(liftSession: LiftSession, _) =>
      context.children.foreach(_ ! liftSession)
      // Zustand beibehalten
      stay
  }

  // Zustands-Automaten initialisieren
  initialize
}

/**
 * AntScout-Factory.
 */
object AntScout {

  /**
   * Aktor-Name
   */
  val ActorName = "antScout"

  /**
   * Uninitialsiert.
   */
  case object Uninitialized extends AntScoutState

  /**
   * Initialiserung anstoÃŸen.
   */
  case object Initialize extends AntScoutMessage

  /**
   * AntNodeSupervisor-Initialisierung.
   */
  case object InitializingAntNodeSupervisor extends AntScoutState

  /**
   * RoutingService-Initialisierung.
   */
  case object InitializingRoutingService extends AntScoutState

  /**
   * RoutingService initialisiert.
   */
  case object RoutingServiceInitialized extends AntScoutMessage

  // AntScout-Aktor erzeugen
  system.actorOf(Props[AntScout], AntScout.ActorName)

  /**
   * Initialisiert AntScout.
   */
  def init() {
    system.actorFor(Iterable("user", ActorName)) ! Initialize
  }

  /**
   * FÃ¤hrt AntScout herunter.
    */
  def shutDown() {
    system.shutdown()
  }
}

/**
 * Gemeinsame Basis fÃ¼r die AntScout-Nachrichten.
 */
sealed trait AntScoutMessage

/**
 * Gemeinsame Basis fÃ¼r die AntScout-ZustÃ¤nde.
 */
sealed trait AntScoutState
