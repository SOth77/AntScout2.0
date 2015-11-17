package de.fhwedel.antscout
package routing

import antnet._
import dijkstra._
import scala.util.control.Breaks._
import akka.actor.{ ActorRef, ActorLogging, Actor }
import collection.mutable
import antnet._
import de.fhwedel.antscout
import net.liftweb.common.{ Empty, Full, Box }
import scala.collection.mutable.LinkedList

class DijkstraRouting extends Actor with ActorLogging{
  
  val mapEdge = mutable.Map[EdgeImpl, AntWay]()
  val mapNode = mutable.Map[map.Node,NodeImpl]()
  val target =  Graph.Node
  val start = Graph.Node
  
  def findPath(source: map.Node, destination: map.Node): Box[Seq[AntWay]] = {
    val result = new LinkedList[Antway]()
    val g = transformGraph()
    //val so = transformActor(source)
    //val de = transformActor(destination)   
    
    //TODO: Umwandlung der Aktoren in Nodes?
    val (start, target) = (mapNode.get(source), mapNode.get(destination))
    val dijkstra = new Dijkstra[g.type](g)
    
    // Halt when target becomes settled
    dijkstra.stopCondition = (S, D, P) => !S.contains(target)
    
    val (distance, path) = dijkstra.compute(start, target)    
    var shortest = List(target)
    while(shortest.head != start) {
      shortest ::= path(shortest.head)
    }
    //TODO: Definitv eine effizientere Lösung an dieser Stelle!
    while (shortest.last != shortest.head) {  
      breakable { for(x <- edges) {
        if  (((x.a or x.b) == shortest.head) 
        and ((x.a or x.b) == shortest.drop(1).head)){
          result = result :+ (mapEdge.get(x))
          break
        }
      }}  
    }
    return new Box(result)   
  }
  
  def transformGraph () : WeightedGraph = {
    val graph = new WeightedGraph(0)    
    AntMap.notes.foreach((i : map.Node) => mapNode.+=(i, graph.addNode))      
    for ((a,b) <- AntMap.outgoingWays) {
      b.foreach((c : AntWay) => mapEdge.+=(mapNode.get(a).connectWith(mapNode.get(c.endNode(a))).setWeight(c.tripTime), c))
    }      
    return graph
  }
  
  //def transformActor(ac : ActorRef): Node = {
    
  //}
  
  
}







