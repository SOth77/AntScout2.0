package de.fhwedel.antscout
package dijkstra

class ConcreteUndirectedGraph extends UndirectedGraph {
  type Node = NodeImpl
  type Edge = EdgeImpl
  protected def newNode: Node = new Node
  protected def newEdge(one: Node, other: Node): Edge =
    new Edge(one, other)
}