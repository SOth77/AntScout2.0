package de.fhwedel.antscout
package dijkstra

import annotation.tailrec
import collection.mutable
import collection.mutable.MutableList
import antnet.{ AntNode, AntWay, AntMap }
import akka.actor.{ ActorRef, ActorLogging, Actor }
import de.fhwedel.antscout
import net.liftweb.common.{ Empty, Full, Box }
import net.liftweb.http.{ NamedCometListener, S, LiftSession }
import de.fhwedel.antscout._

class DijkstraService extends Actor with ActorLogging {
  import DijkstraService._
  /**
   * Lift-Session
   */
  var liftSession: Option[LiftSession] = None

  val mapEdge = new mutable.HashMap[(Int, Int), AntWay]()
  val mapNode = new mutable.HashMap[map.Node, Int]()
  val graph = new Graph()

  /**
   * Sucht einen Pfad von einem Quell- zu einem Ziel-Knoten.
   */
  def findPath(source: String, destination: String): Box[Seq[AntWay]] = {
    val (start, target) = transformGraph(source, destination)
    graph.dijkstra(start, target) match 
    {
      case Some((path, distance)) => return Full(result(path))
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
    log.info("erti transform: Vor der 1. Schleife, mapEdge Size: %d".format(mapEdge.size))
    AntMap.nodes.foreach((i: map.Node) => mapNode.put(i, graph.mkNode))
    log.info("erti transform, AntMap Size: %d, mapNode Size: %d".format(AntMap.nodes.size, mapNode.size))
    for ((a, b) <- AntMap.outgoingWays) {
      for (c <- b) {
        val startNode = mapNode(a)
        //log.info("erti transform, startNode: %d".format(startNode))
        val endNode = mapNode(c.endNode(a))
        //log.info("erti transform, endNode: %d".format(endNode))
        graph.setDistance(startNode, endNode, c.tripTime)
        mapEdge += (((startNode,endNode), c))
      }
    }
    log.info("erti transform: Zwischen den Schleifen, mapEdge Size: %d".format(mapEdge.size))
    for ((d, e) <- AntMap.incomingWays) {
      for (f <- e) {
        val startNode = mapNode(d)
        val endNode = mapNode(f.endNode(d))
        graph.setDistance(startNode, endNode, f.tripTime)
        mapEdge += (((startNode,endNode), f))
      }
    }
    log.info("erti transform: Nach der 2. Schleife, mapEdge Size: %d".format(mapEdge.size))

    val source2 = AntMap.nodes.find(x => x.id == source).getOrElse(new map.Node(source))
    log.info("erti transform, source2: %s".format(source2.id))
    val destination2 = AntMap.nodes.find(x => x.id == destination).getOrElse(new map.Node(destination))
    log.info("erti transform, destination2: %s".format(destination2.id))
    val start = mapNode(source2)
    log.info("erti transform, start: %d".format(start))
    val target = mapNode(destination2)
    log.info("erti transform, target: %d".format(target))
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
      if (actNode != i)
      {
        actNode = i
        result += (mapEdge((lastNode,actNode)))
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