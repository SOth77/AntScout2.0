package de.fhwedel.antscout
package dijkstra

import annotation.tailrec
import collection.mutable
import collection.mutable.MutableList
import antnet.{ AntNode, AntWay, AntMap }
import akka.actor.{ Cancellable, ActorRef, ActorLogging, Actor }
import de.fhwedel.antscout
import net.liftweb.common.{ Empty, Full, Box }
import net.liftweb.http.{ NamedCometListener, S, LiftSession }
import de.fhwedel.antscout._
import scala.concurrent._
import java.util.concurrent.TimeUnit

/**
 * Beantwortet Anfragen nach dem aktuellen Pfad von einem Quell- zu einem
 * Ziel-Knoten und versorgt das Front-End mit immer aktuellen Pfaden. Stößt
 * die Transformation des Graphen zwischen zwei verschiedenen Repräsentationen an.
 */
class DijkstraService extends Actor with ActorLogging {
  import DijkstraService._

  /**
   * Lift-Session
   */
  var liftSession: Option[LiftSession] = None

  /**
   * Tabelle zur Transformation von Kanten
   */
  val mapEdge = new mutable.HashMap[(Int, Int), AntWay]()
  
  /**
   * Tabelle zur Transformation von Knoten 
   */
  val mapNode = new mutable.HashMap[map.Node, Int]()
  
  /**
   * Der transformierte Graph
   */
  val graph = new Graph()

  /**
   * Sucht einen Pfad von einem Quell- zu einem Ziel-Knoten.
   */
  def findPath(source: String, destination: String): Box[Seq[AntWay]] = {
    val (start, target) = transformGraph(source, destination)
    graph.dijkstra(start, target) match {
      case Some((path, distance)) => {
        return Full(result(path))
      }
      case None => return Empty
    }
  }

  /**
   * Transformiert den Graphen in eine Form, auf die der Dijkstra ausgeführt werden kann
   */
  def transformGraph(source: String, destination: String): (Int, Int) = {
    mapEdge.clear()
    mapNode.clear()
    graph.clear()
    AntMap.nodes.foreach((i: map.Node) => mapNode.put(i, graph.mkNode))
    for ((a, b) <- AntMap.outgoingWays) {
      for (c <- b) {
        val startNode = mapNode(a)
        val endNode = mapNode(c.endNode(a))
        graph.setDistance(startNode, endNode, c.tripTime)
        mapEdge += (((startNode, endNode), c))
      }
    }
    for ((d, e) <- AntMap.incomingWays) {
      for (f <- e) {
        val startNode = mapNode(d)
        val endNode = mapNode(f.endNode(d))
        graph.setDistance(startNode, endNode, f.tripTime)
        mapEdge += (((startNode, endNode), f))
      }
    }

    //Transformiert Start- und Zielpunkt
    val source2 = AntMap.nodes.find(x => x.id == source).getOrElse(new map.Node(source))
    val destination2 = AntMap.nodes.find(x => x.id == destination).getOrElse(new map.Node(destination))
    val start = mapNode(source2)
    val target = mapNode(destination2)
    return (start, target)
  }

  /**
   * Transformiert das Ergebnis in eine Form, die mit der des Ameisenalgorithmus vergleichbar ist
   */
  def result(path: List[Int]): Seq[AntWay] = {
    val result = new MutableList[AntWay]()
    var actNode = path.head
    var lastNode = path.head
    for (i <- path)
      if (actNode != i) {
        actNode = i
        result += (mapEdge((lastNode, actNode)))
        lastNode = i
      }
    return result
  }

  /**
   * Initialisiert den Dijkstra-Service.
   */
  def init() {
    log.info("Initialized")
    context.parent ! AntScout.ServiceInitialized
  }

  /**
   * Callback-Funktion, die vor dem Start des Aktors ausgeführt wird.
   */
  override def preStart() {
    log.info("Initializing")
  }

  protected def receive = {
    // Anfrage nach einem Pfad
    case FindPath(source, destination) =>
      // Pfad suchen
      val path = findPath(source, destination)
      for {
        liftSession <- liftSession
      } yield {
        S.initIfUninitted(liftSession) {
          // Pfad in die Session schreiben
          antscout.Path(path)
        }
      }
      // Pfad zurücksenden
      sender ! path
    // Initialisierung
    case Initialize =>
      init()
    // Lift-Session
    case liftSession: LiftSession =>
      this.liftSession = Some(liftSession)
    case m: Any =>
      log.warning("Unknown message: %s" format m.toString)
  }
}

/**
 * Dijkstra-Factory.
 */
object DijkstraService {

  /**
   * Aktor-Name
   */
  val ActorName = "dijkstra"

  /**
   * Anfrage nach einem Pfad.
   *
   * @param source Quelle
   * @param destination Ziel
   */
  case class FindPath(source: String, destination: String)

  /**
   * Initialisierung
   */
  case object Initialize

  /**
   * Pfad.
   *
   * @param path Pfad.
   */
  case class Path(path: Box[Seq[AntWay]])
}  