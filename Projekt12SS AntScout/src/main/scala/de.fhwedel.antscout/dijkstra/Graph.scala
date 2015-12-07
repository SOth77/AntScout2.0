package de.fhwedel.antscout
package dijkstra

abstract class Graph {
  type Edge <: IEdge
  type Node <: INode
  abstract class INode {
    def connectWith(node: Node): Edge
  }
  abstract class IEdge {
    def a: Node
    def b: Node
    def opposite(n: Node): Option[Node]
  }
  def nodes: List[Node]
  def edges: List[Edge]
  def addNode: Node
}