package de.fhwedel.antscout
package dijkstra

import scala.collection.mutable.Set
import scala.collection.mutable.Map
import net.liftweb.common.{ Full, Box, Logger }

class Graph extends Logger {
  type Node = Int
  type Distance = Double

  var nextNode: Node = 0
  var matrix = Map[Node, Map[Node, Distance]]()

  def mkNode(): Node =
    {
      var node: Node = nextNode
      matrix += (node -> Map())
      nextNode += 1
      node
    }

  def getNodes() = matrix.keys

  def setDistance(from: Node, to: Node, distance: Distance) =
    {
      matrix.get(from) match {
        case Some(m) => matrix += (from -> (m + (to -> distance)))
        case None => // if this happens, you used an undefined node
      }
    }

  def getDistances(from: Node): Map[Node, Distance] =
    {
      matrix.get(from) match {
        case Some(ds) => ds
        case None => Map()
      }
    }

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

  def dijkstra(from: Node, to: Node): Option[(List[Node], Distance)] =
    {
      val unexplored: Set[Node] = Set()

      // shortest known distance to nodes and node on the way just before
      val distances: Map[Node, (Node, Distance)] = Map()
      
      info("erti: Dijkstra-Start, from: %d, to: %d".format(from, to))

      // the start node is always explored and its distance is 0
      distances += (from -> (from, 0))

      // all other nodes are unexplored
      getNodes().foreach((n: Node) => unexplored += n)
      unexplored -= from
      info("erti Dijkstra, unexplored Size: %d".format(unexplored.size))

      // and we know the distances to all neighbours of from
      // we're also setting the before nodes for every edge that starts at from
      getDistances(from).foreach((edge: (Node, Distance)) =>
        {
          val (end, distance) = edge
          distances += (end -> (from, distance))
        })
      info("erti, distances Size: %d".format(distances.size))

      // subset the distances to the unexplored nodes
      // we need this to terminate the search in case there is no path
      var distancesToUnexplored = distances filterKeys unexplored
      info("erti, distancesToUnexplored Size: %d".format(distancesToUnexplored.size))
      var counter = 0

      info("erti: Vor der Schleife")
      // now we repeatedly choose the closest unexplored node and explore it
      while ((unexplored contains to) && !distancesToUnexplored.isEmpty) {
        counter = counter + 1
        info("erti: Schleifenzähler %d".format(counter))
        // select the closest unexplored node
        val (closest, (_, d)) = distancesToUnexplored.minBy(_._2._2)
        info("erti, closest: %d".format(closest))

        // now update all distances for which there is a shorter way through closest
        // only update unexplored nodes and don't forget the before nodes
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

        // set the node to be explored
        unexplored -= closest
        distancesToUnexplored = distances filterKeys unexplored
      }
      info("erti: Nach der Schleife")
      // now we have to check if we found a path and reconstruct its pieces
      distances.get(to) match {
        // yay, there's a path
        case Some((b4, d)) =>
          {
            info("erti: Pfad gefunden")
            var step = b4
            var thePath: List[Node] = List(to)

            // it's ok to loop and discard non-existent branches, we know already we
            // will succeed because we found a path
            while (step != from) {
              thePath = step :: thePath
              step = distances.get(step) match {
                case Some((b4, _)) => b4
                case None => step // this doesn't happen, but if it does, we end in an endless recursion
              }
            }
            Some(from :: thePath, d)
          }
        // no path :(
        case None =>
          {
            info("erti: keinen Pfad gefunden")
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