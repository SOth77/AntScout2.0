//package de.fhwedel.antscout
//package dijkstra
//
//abstract class UndirectedGraph extends Graph {
//  class EdgeImpl(one: Node, other: Node) extends IEdge {
//    def a = one
//    def b = other
//    def opposite(n: Node): Option[Node] =
//      if(n == a) Some(b)
//      else if(n == b) Some(a)
//      else None
//  }
//
//  class NodeImpl extends INode {
//    this: Node =>
//    def connectWith(node: Node): Edge = {
//      val edge = newEdge(this, node)
//      edges = edge :: edges;
//      edge
//    }
//    override def toString:String =
//      (nodes.length - nodes.findIndexOf(this == _)).toString()
//  }
//
//  protected def newNode: Node
//  protected def newEdge(one: Node, other: Node): Edge
//
//  var nodes: List[Node] = Nil
//  var edges: List[Edge] = Nil
//
//  def addNode: Node = {
//    val node = newNode
//    nodes = node :: nodes
//    node
//  }
//}