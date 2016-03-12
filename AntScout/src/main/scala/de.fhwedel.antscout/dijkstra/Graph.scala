package de.fhwedel.antscout
package dijkstra

import scala.collection.mutable.Set
import scala.collection.mutable.Map
import net.liftweb.common.{ Full, Box, Logger }

/**
 * Implementierung eines Graphen
 */
class Graph extends Logger {
  type Node = Int
  type Distance = Double

  /**
   * ID des als nächstes zu erzeugenden Knotens
   */
  var nextNode: Node = 0
  
  /**
   * 2-Dimensionale Tabelle, die Knoten auf Knoten auf das Kantengewicht abbildet
   */
  var matrix = Map[Node, Map[Node, Distance]]()

  /**
   * Erzeugen eines neuen Knotens
   */
  def mkNode(): Node =
    {
      var node: Node = nextNode
      matrix += (node -> Map())
      nextNode += 1
      node
    }

  /**
   * Getter für alle Knoten
   */
  def getNodes() = matrix.keys

  /**
   * Definiert eine Kante zwischen zwei Knoten mit bestimmter Fahrzeit
   */
  def setDistance(from: Node, to: Node, distance: Distance) =
    {
      matrix.get(from) match {
        case Some(m) => matrix += (from -> (m + (to -> distance)))
        case None => // dies kann nur bei einem undefinierten Knoten geschehen
      }
    }

  /**
   * Getter für alle Kanten von einem Knoten
   */
  def getDistances(from: Node): Map[Node, Distance] =
    {
      matrix.get(from) match {
        case Some(ds) => ds
        case None => Map()
      }
    }

  /**
   * Getter für die Fahrzeit einer Kante
   */
  def getDistance(from: Node, to: Node): Option[Distance] =
    {
      matrix.get(from) match {
        case Some(ds) => ds.get(to)
        case None => None
      }
    }

  def clear() =
    {
      nextNode = 0
      matrix.clear()
    }

  /**
   * Führt den Dijkstra-Algorithmus auf dem Graphen mit den gegebenen Endpunkten aus
   */
  def dijkstra(from: Node, to: Node): Option[(List[Node], Distance)] =
    {
      val unexplored: Set[Node] = Set()

      // Kürzeste bekannte Distanz zu einem Knoten und der Knoten auf diesem Weg direkt davor
      val distances: Map[Node, (Node, Distance)] = Map()

      // Der Startknoten ist immer erforschtt und seine Distanz beträgt 0
      distances += (from -> (from, 0))

      // Alle anderen Knoten sind unerforscht
      getNodes().foreach((n: Node) => unexplored += n)
      unexplored -= from

      // Wir kennen die Distanz zu allen Nachbarn von from
      // Wir setzen die Vorgängerknoten für alle Kanten, die bei from starten
      getDistances(from).foreach((edge: (Node, Distance)) =>
        {
          val (end, distance) = edge
          distances += (end -> (from, distance))
        })

      // Dies ist nötig, um eine Terminierung auch dann zu garantieren, wenn es keinen Pfad gibt 
      var distancesToUnexplored = distances filterKeys unexplored
      var counter = 0

      // Wir wählen jetzt wiederholt den dichtesten unerforschten Knoten und erforschen ihn
      while ((unexplored contains to) && !distancesToUnexplored.isEmpty) {
        counter = counter + 1
        // es wird der dichteste unerforschte Knoten gewählt
        val (closest, (_, d)) = distancesToUnexplored.minBy(_._2._2)

        // Jetzt werden alle Distanzen aktualisiert, für die ein kürzerer Weg durch closest führt
        // Nur unerforschte Knoten werden aktualisiert
        getDistances(closest).foreach((edge: (Node, Distance)) =>
          {
            val (end, additionalDistance) = edge
            val newDistance = d + additionalDistance

            distances.get(end) match {
              case Some((_, oldDistance)) =>
                if (newDistance < oldDistance)
                  distances += (end -> (closest, newDistance))
              case None =>
                distances += (end -> (closest, newDistance))
            }
          })
          
        // Der  Knoten wird als erforscht gesetzt  
        unexplored -= closest
        distancesToUnexplored = distances filterKeys unexplored
      }
      // Nun muss geprüft werden, ob ein Pfad gefunden wurde. 
      // Falls ja, müssen die Teilabschnitte zurückverfolgt werden 
      distances.get(to) match {
        //Pfad gefunden
        case Some((b4, d)) =>
          {
            var step = b4
            var thePath: List[Node] = List(to)

            // Es stört nicht, nicht vorhandene Teile zu entfernen, da
            // wir bereits wissen, dass ein Pfad existiert
            while (step != from) {
              thePath = step :: thePath
              step = distances.get(step) match {
                case Some((b4, _)) => b4
                case None => step // dies kann nicht vorkommen und würde in einer Endlosrekusion enden
              }
            }
            Some(from :: thePath, d)
          }
        // kein Weg gefunden
        case None =>
          {
            None
          }
      }

    }
}


// -----------------------------------------------------------------------------

// create a graph
//val g = new Graph()

// create some nodes
//val from = g.mkNode()
//val n1   = g.mkNode()
//val n2   = g.mkNode()
//val to   = g.mkNode()


// connect the nodes such that the shortest way is from -> n1 -> n2 -> to, such thath
//
//        1         1         1
// from -----> n1 -----> n2 -----> to
//  | |                  ^         ^
//  | |------------------|         |
//  |          21                  |
//  |                              |
//  |------------------------------|
//                  42

//g.setDistance(from, n1,  1)
//g.setDistance(from, n2, 21)
//g.setDistance(from, to, 42)
//g.setDistance(n1,   n2,  1)
//g.setDistance(n2,   to,  1)
//
//g.dijkstra(from, to) match
//{
//  case Some((path, distance)) =>
//  {
//    println("Found path " + path.toString())
//    println("The distance is " + distance.toString())
//  }
//  case None => println("No path could be found.")
//}