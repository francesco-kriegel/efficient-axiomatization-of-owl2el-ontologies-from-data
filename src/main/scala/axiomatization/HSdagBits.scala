
package de.tu_dresden.inf.lat
package axiomatization

import com.google.common.collect.Sets

import collection.JavaConverters.*
import scala.util.Random
import collection.parallel.CollectionConverters.SetIsParallelizable
import scala.annotation.tailrec
//import collection.parallel.CollectionConverters.IterableIsParallelizable
import scala.collection.mutable

import de.tu_dresden.inf.lat.axiomatization.Util._

object HSdagBits {

  def allMinimalHittingSets(hypergraph: collection.Set[collection.BitSet]): collection.Set[collection.BitSet] = {
    allMinimalHittingSets(hypergraph.iterator)
  }

  def allMinimalHittingSets(hypergraph: Iterator[collection.BitSet]): collection.Set[collection.BitSet] = {
    val hstree = new HSdagBitsPar(hypergraph)
    hstree.populate()
    hstree.allMinimalHittingSets()
  }

}

private class HSdagBitsPar(F: Iterator[collection.BitSet]) {

  private val seenHyperedges = new collection.mutable.HashSet[collection.BitSet]

  private def hasNextHyperedge(): Boolean = {
    F.hasNext
  }

  private def getNextHyperedge(): collection.BitSet = {
    val nextHyperedge = F.next()
    seenHyperedges.add(nextHyperedge)
    nextHyperedge
  }

  private def findHyperedge(condition: collection.BitSet ⇒ Boolean): Option[collection.BitSet] = {
    def scanF(): Option[collection.BitSet] = {
      if (hasNextHyperedge()) {
        val nextHyperedge = getNextHyperedge()
        if (condition(nextHyperedge))
          Some(nextHyperedge)
        else
          scanF()
      } else {
        None
      }
    }
    F.synchronized {
      (seenHyperedges find condition) orElse scanF()
    }
  }

  private def ignoreHyperedge(hyperedge: collection.BitSet): Boolean = {
    seenHyperedges.remove(hyperedge)
  }

  sealed trait Label
  case class HyperedgeNode(hyperedge: collection.BitSet) extends Label
  case class LeafNode() extends Label
  case class ClosedNode() extends Label
  case class NewNode() extends Label

  private class Node(
    var label: Label,
    val H:     collection.BitSet) {

    val predecessors =
      new mutable.HashMap[Node, mutable.BitSet] //with mutable.MultiMap[Node, Int]
    val successors =
      new mutable.HashMap[Node, mutable.BitSet] //with mutable.MultiMap[Node, Int]

    def isNew(): Boolean = {
      label match {
        case NewNode() ⇒ true
        case default   ⇒ false
      }
    }

    def isLeaf(): Boolean = {
      label match {
        case LeafNode() ⇒ true
        case default    ⇒ false
      }
    }

    def isClosed(): Boolean = {
      label match {
        case ClosedNode() ⇒ true
        case default      ⇒ false
      }
    }

    def hyperedge(): Option[collection.BitSet] = {
      label match {
        case HyperedgeNode(hyperedge) ⇒ Some(hyperedge)
        case default                  ⇒ None
      }
    }

    def ancestors(): collection.Set[Node] = {
      predecessors.keySet ++ predecessors.keySet.par.flatMap(_.ancestors())
    }

    def descendants(): collection.Set[Node] = {
      successors.keySet ++ successors.keySet.par.flatMap(_.descendants())
    }

  }

  private val rootNode: Node = new Node(new NewNode(), scala.collection.immutable.BitSet.empty)

  private def addEdge(source: Node, edgeLabel: Int, target: Node): Unit = {
    source.successors.synchronized {
      source.successors.addBinding(target, edgeLabel)
    }
    target.predecessors.synchronized {
      target.predecessors.addBinding(source, edgeLabel)
    }
  }

  private def removeEdge(source: Node, edgeLabel: Int, target: Node): Unit = {
    source.successors.synchronized {
      source.successors.removeBinding(target, edgeLabel)
    }
    target.predecessors.synchronized {
      target.predecessors.removeBinding(source, edgeLabel)
    }
  }

  private def removeNode(node: Node): Unit = {
    node.predecessors.foreachBinding((predecessor, edgeLabel) ⇒ removeEdge(predecessor, edgeLabel, node))
    node.successors.foreachBinding((successor, edgeLabel) ⇒ removeEdge(node, edgeLabel, successor))
  }

  private def activeNodes(): collection.Set[Node] = {
    rootNode.descendants()
  }

  def allMinimalHittingSets(): collection.Set[collection.BitSet] = {
    activeNodes() filter { _.isLeaf() } map { _.H }
  }

  def populate(): Unit = {
    expand(Set(rootNode))
  }

  @tailrec
  private def expand(nodes: collection.Set[Node]): Unit = {
    val nextLevel = Sets.newConcurrentHashSet[Node]().asScala
    nodes.par.foreach(node ⇒ {
      if (activeNodes().par.exists(activeNode ⇒ activeNode.isLeaf() && (activeNode.H strictSubsetOf node.H))) {
        node.label = new ClosedNode()
      } else {
        node.label =
          findHyperedge(hyperedge ⇒ (hyperedge intersect node.H).isEmpty)
            .map(hyperedge ⇒ { pruneWith(hyperedge); new HyperedgeNode(hyperedge) })
            .getOrElse(new LeafNode())
        node.hyperedge().foreach(hyperedge ⇒ {
          hyperedge.par.foreach(edgeLabel ⇒ {
            val successor =
              activeNodes().par.find(activeNode ⇒ activeNode.H equals (node.H + edgeLabel))
                .getOrElse(new Node(new NewNode(), node.H + edgeLabel))
            addEdge(node, edgeLabel, successor)
            if (successor.isNew())
              nextLevel.add(successor)
          })
        })
      }
    })
    if (!nextLevel.isEmpty)
      expand(nextLevel)
  }

  private def pruneWith(hyperedge: collection.BitSet): Unit = {
    activeNodes().par.foreach(activeNode ⇒ {
      activeNode.label match {
        case HyperedgeNode(otherHyperedge) ⇒ {
          if (hyperedge strictSubsetOf otherHyperedge) {
            activeNode.label = new HyperedgeNode(hyperedge)
            val possiblyToBeRemoved = new collection.mutable.HashSet[Node]
            activeNode.successors.foreachBinding((successor, edgeLabel) ⇒ {
              if ((otherHyperedge -- hyperedge) contains edgeLabel) {
                removeEdge(activeNode, edgeLabel, successor)
                possiblyToBeRemoved += successor
                possiblyToBeRemoved ++= successor.descendants()
              }
            })
            val certainlyToBeRemoved = fixedPoint[collection.mutable.HashSet[Node]](probablyToBeRemoved ⇒ {
              val moreProbablyToBeRemoved = probablyToBeRemoved.clone
              moreProbablyToBeRemoved.retain(node ⇒ node.predecessors.keySet subsetOf probablyToBeRemoved)
              moreProbablyToBeRemoved
            }, _ equals _)(possiblyToBeRemoved)
            certainlyToBeRemoved.par.foreach(node ⇒ removeNode(node))
            ignoreHyperedge(otherHyperedge)
          }
        }
        case default ⇒ {}
      }
    })
  }

}
