package de.fhwedel.antscout
package dijkstra

import scala.collection.mutable._
import antnet._
import scala.util.control.Breaks._
import akka.actor.{ ActorRef, ActorLogging, Actor }
import antnet.{ AntNode, AntWay }
import collection.mutable
import de.fhwedel.antscout
import net.liftweb.common.{ Empty, Full, Box }
import map._
import net.liftweb.http.{ NamedCometListener, S, LiftSession }

class Dijkstra extends Actor with ActorLogging {

  import Dijkstra._

  type Node = WeightedGraph#Node
  type Edge = WeightedGraph#Edge

  var mapEdge = mutable.HashMap[Edge, AntWay]()
  var mapNode = mutable.HashMap[map.Node, Node]()
  var graph = new WeightedGraph(1)

  /**
   * StopCondition provides a way to terminate the algorithm at a certain
   * point, e.g.: When target becomes settled.
   */
  type StopCondition = (Set[Node], Map[Node, Int], Map[Node, Node]) => Boolean

  /**
   * By default the Dijkstra algorithm processes all nodes reachable from
   * <code>start</code> given to <code>compute()</code>.
   */
  val defaultStopCondition: StopCondition = (_, _, _) => true
  var stopCondition = defaultStopCondition

  def transformGraph(source: String, destination: String): (Node, Node) = {
    graph = new WeightedGraph(1)
    mapEdge = new mutable.HashMap[Edge, AntWay]()
    mapNode = new mutable.HashMap[map.Node, Node]()
    AntMap.nodes.foreach((i: map.Node) => mapNode.put(i, graph.addNode))
    for ((a, b) <- AntMap.outgoingWays) {
      for (c <- b) {
        var newEdge = mapNode(a).connectWith(mapNode(c.endNode(a)))
        newEdge.setWeight(c.tripTime)
        mapEdge += ((newEdge, c))
      }
    }
    val source2 = AntMap.nodes.find(x => x.id == source).getOrElse(new map.Node(source))
    val destination2 = AntMap.nodes.find(x => x.id == destination).getOrElse(new map.Node(destination))
    val start = mapNode(source2)
    val target = mapNode(destination2)
    return (start, target)
  }

  def result(shortest: List[Node]): MutableList[AntWay] = {
    //TODO: Definitv eine effizientere Lösung an dieser Stelle als Doppelschleife über alles (z.B. neue Strukturen dafür schaffen)
    val result = new MutableList[AntWay]()
    while (shortest.last != shortest.head) {
      breakable {
        for (x <- graph.edges) {
          if ((x.a == shortest.head || x.b == shortest.head)
            & (x.a == shortest.drop(1).head || x.b == shortest.drop(1).head)) {
            result += (mapEdge(x))
            break
          }
        }
      }
    }
    return result
  }

  def compute(start: Node, target: Node): (Map[Node, Int], Map[Node, Node]) = {
    var queue: Set[Node] = new HashSet()
    var settled: Set[Node] = new HashSet()
    var distance: Map[Node, Int] = new HashMap()
    var path: Map[Node, Node] = new HashMap()
    queue += start
    distance(start) = 0

    while (!queue.isEmpty && stopCondition(settled, distance, path)) {
      val u = extractMinimum(queue, distance)
      settled += u
      relaxNeighbors(u, queue, settled, distance, path)
    }

    return (distance, path)
  }

  /**
   * Finds element of <code>Q</code> with minimum value in D, removes it
   * from Q and returns it.
   */
  protected def extractMinimum[T](Q: Set[T], D: Map[T, Int]): T = {
    var u = Q.first
    Q.foreach((node) => if (D(u) > D(node)) u = node)
    Q -= u
    return u;
  }

  /**
   * For all nodes <code>v</code> not in <code>S</code>, neighbors of
   * <code>u</code>}: Updates shortest distances and paths, if shorter than
   * the previous value.
   */
  protected def relaxNeighbors(u: Node, Q: Set[Node], S: Set[Node],
    D: Map[Node, Int], P: Map[Node, Node]): Unit = {
    for (edge <- graph.edges if (edge.a == u || edge.b == u)) {
      var v = if (edge.a == u) edge.b else edge.a
      if (!S.contains(v)) {
        if (!D.contains(v) || D(v) > D(u) + edge.getWeight) {
          D(v) = D(u) + edge.getWeight
          P(v) = u
          Q += v
        }
      }
    }
  }

  def findPath(source: String, destination: String): Box[Seq[AntWay]] = {
    val (start, target) = transformGraph(source, destination)
    // Halt when target becomes settled
    stopCondition = (S, D, P) => !S.contains(target)

    val (distance, path) = compute(start, target)
    var shortest = List(target)
    while (shortest.head != start) {
      shortest ::= path(shortest.head)
    }

    return Full(result(shortest))
  }

  /**
   * Lift-Session
   */
  var liftSession: Option[LiftSession] = None

  protected def receive = {
    // Anfrage nach einem Pfad
    case FindPath(source, destination) =>
      // Pfad suchen
      val path = findPath(source, destination)
      for {
        liftSession <- liftSession
      } yield {
        // Pfad in die Session schreiben
        antscout.Path(path)
      }
      // Pfad zurücksenden
      sender ! path
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
object Dijkstra {

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
   * Pfad.
   *
   * @param path Pfad.
   */
  case class Path(path: Box[Seq[AntWay]])
}  