package de.fhwedel.antscout

import antnet.{ AntMap, AntWayData }
import osm.OsmMap
import routing.RoutingService
import dijkstra.DijkstraService
import antnet.JamGen
import akka.actor._
import net.liftweb.http.LiftSession
import antnet.AntNodeSupervisor
import akka.actor.Props
import akka.util._
import scala.concurrent._
import java.util.concurrent.TimeUnit
import net.liftweb.util.TimeHelpers
import collection.mutable

/**
 * Initialisiert die Anwendung mit Hilfe eines Zustands-Automaten.
 */
class AntScout extends Actor with FSM[AntScoutState, Unit] {

  import AntScout._
  
   /**
   * Cancellabeles werden beim Erzeugen von Schedulern zurückgegeben und erlauben es diese zu stoppen.
   */
  val cancellables = mutable.Set[Cancellable]()

  // AntNodeSupervisor erzeugen
  context.actorOf(Props[AntNodeSupervisor].withDispatcher("ant-node-supervisor-dispatcher"),
    AntNodeSupervisor.ActorName)
  // Routing-Service erzeugen
  context.actorOf(Props[RoutingService], RoutingService.ActorName)
  // Dijkstra erzeugen
  context.actorOf(Props[DijkstraService], DijkstraService.ActorName)
  // Staugenerator erzeugen
  context.actorOf(Props[JamGen], JamGen.ActorName)

  // Start-Zustand
  startWith(Uninitialized, Unit)

  // Uninitialisiert
  when(Uninitialized) {
    // Initialisierung anstoÃŸen
    case Event(Initialize, _) =>
      // OsmMap initialisieren
      OsmMap(Settings.Map)
      if (Settings.Dji) {
        // DijkstraService-Initialisierung anstoÃŸen
        context.actorFor(DijkstraService.ActorName) ! DijkstraService.Initialize
      } else {
        // RoutingService-Initialisierung anstoÃŸen
        context.actorFor(RoutingService.ActorName) ! RoutingService.Initialize
      }

      // Zustand wechseln
      goto(InitializingService)
  }

  // DijkstraService-Initialisierung
  when(InitializingService) {
    // DijkstraService initialisiert
    case Event(ServiceInitialized, _) =>
      // Ant-Weg-Daten berechnen
      val antWayData = AntMap.prepare
      // Ant-Knoten berechnen
      AntMap.computeNodes(antWayData)
      // AntNodeSupervisor-Initialisierung anstoÃŸen
      context.actorFor(AntNodeSupervisor.ActorName) ! AntNodeSupervisor.Initialize(antWayData)
      if (Settings.Jamgen) {
        // Stauerzeugung-Initialisierung anstoßen
        context.actorFor(JamGen.ActorName) ! JamGen.Initialize
      }
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
      if (Settings.Dji == false) {
        // Initialisierung der Ant-Knoten anstoÃŸen
        context.actorFor(AntNodeSupervisor.ActorName) ! AntNodeSupervisor.InitializeNodes
      }
      //Erzeugen des Schedules für die Staus
      if (Settings.Jamgen) {
        cancellables += context.system.scheduler.schedule(Duration.Zero, Duration(Settings.Frequency,
            TimeUnit.MILLISECONDS), context.actorFor(JamGen.ActorName), JamGen.StartGen())
      }
      // In diesem Zustand bleiben
      stay()
  }
  
  /**
   * Event-Handler, der nach dem Stoppen des Aktors augeführt wird.
   */
  override def postStop() {
    // Alle schedule-Aktionen stoppen
    for (cancellable <- cancellables)
      cancellable.cancel()
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
   * RoutingService initialisiert.
   */
  case object ServiceInitialized extends AntScoutMessage

  /**
   * Staugenerator initialisiert.
   */
  case object JamGenInitialized extends AntScoutMessage

  /**
   * DijkstraService-Initialisierung.
   */
  case object InitializingService extends AntScoutState

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
