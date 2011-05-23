package xsbti.api

import java.util.IdentityHashMap

object GraphCompare {
  final def sameIterator[T](a: Iterator[T], b: Iterator[T])(eq: (T,T) => Boolean): Boolean =
		(a.length == b.length) && (a zip b).forall(eq.tupled)

  trait Node {
    def children: Iterator[Node]
    def equiv(node: Node): Boolean
  }

  class LeafNode extends Node {
    def equiv(node: Node): Boolean = this == node
    def children: Iterator[Node] = Iterator.empty
  }

  trait ProductNode extends Product with Node {
    def children: Iterator[Node] = this.productIterator.collect { case node: Node => node }
    def equiv(node: Node): Boolean = {
      node match {
        case prodNode: ProductNode => {
          if (this.productArity != prodNode.productArity) return false
          var i = 0
          while (i < this.productArity) {
            val thisElem = this.productElement(i)
            val thatElem = prodNode.productElement(i)
            if (!thisElem.isInstanceOf[Node] && !thatElem.isInstanceOf[Node]) {
              if (thisElem != thatElem) return false
            }
            i += 1
          }
          true
        }
        case _ => false
      }
    }
  }

  final class MyNodeCompare(thisVars: TagTypeVariables.TypeVars, thatVars: TagTypeVariables.TypeVars) {
    def apply(v1: Node, v2: Node): Boolean = {
      if (v1 eq v2) return true
      if (v1.getClass() != v2.getClass()) return false
      (v1, v2) match {
        case (x1: TypedId, x2: TypedId) => (thisVars.get(x1.id) == thatVars.get(x2.id)) && v1.equiv(v2)
        case _ => v1.equiv(v2)
      }
    }
  }

  def compare(eq: (Node, Node) => Boolean,
              filter: (Node) => Boolean,
              marked: IdentityHashMap[Node, Node])(node1: Node, node2: Node): Boolean = {
    if (marked.get(node1) == node2) return true
    if (!eq(node1,node2)) return false
    marked.put(node1, node2)
    sameIterator(node1.children.filter(filter), node2.children.filter(filter))(compare(eq, filter, marked))
  }
}