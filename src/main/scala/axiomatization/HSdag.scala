package de.tu_dresden.inf.lat
package axiomatization

import axiomatization.NestedParallelComputations.*
import axiomatization.Util.*

import com.google.common.collect.Sets

import scala.annotation.tailrec
import scala.collection.JavaConverters.*
import scala.collection.mutable
import scala.util.Random


object HSdag {

  def allMinimalHittingSets[A](hypergraph: collection.Set[collection.Set[A]]): collection.Set[collection.Set[A]] = {
    allMinimalHittingSets(hypergraph.iterator)
  }

  def allMinimalHittingSets[A](hypergraph: Iterator[collection.Set[A]]): collection.Set[collection.Set[A]] = {
    val hstree = new HSdagPar[A](hypergraph)
    hstree.populate()
    hstree.allMinimalHittingSets()
  }

}

private class HSdagPar[A](F: Iterator[collection.Set[A]]) {

  private val seenHyperedges = new collection.mutable.HashSet[collection.Set[A]]

  private def hasNextHyperedge(): Boolean = {
    F.hasNext
  }

  private def getNextHyperedge(): collection.Set[A] = {
    val nextHyperedge = F.next()
    seenHyperedges.add(nextHyperedge)
    nextHyperedge
  }

  private def findHyperedge(condition: collection.Set[A] ⇒ Boolean): Option[collection.Set[A]] = {
    def scanF(): Option[collection.Set[A]] = {
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

  private def ignoreHyperedge(hyperedge: collection.Set[A]): Boolean = {
    seenHyperedges.remove(hyperedge)
  }

  sealed trait Label
  case class HyperedgeNode(hyperedge: collection.Set[A]) extends Label
  case class LeafNode() extends Label
  case class ClosedNode() extends Label
  case class NewNode() extends Label

  private class Node(
    var label: Label,
    val H:     collection.Set[A]) {

    val predecessors =
      new mutable.HashMap[Node, mutable.Set[A]] with mutable.MultiMap[Node, A]
    val successors =
      new mutable.HashMap[Node, mutable.Set[A]] with mutable.MultiMap[Node, A]

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

    def hyperedge(): Option[collection.Set[A]] = {
      label match {
        case HyperedgeNode(hyperedge) ⇒ Some(hyperedge)
        case default                  ⇒ None
      }
    }

    def ancestors(): collection.Set[Node] = {
      predecessors.keySet ++ predecessors.keySet.flatMap(_.ancestors())
    }

    def descendants(): collection.Set[Node] = {
      successors.keySet ++ successors.keySet.flatMap(_.descendants())
    }

  }

  private val rootNode: Node = new Node(new NewNode(), Set.empty[A])

  private def addEdge(source: Node, edgeLabel: A, target: Node): Unit = {
    source.successors.synchronized {
      source.successors.addBinding(target, edgeLabel)
    }
    target.predecessors.synchronized {
      target.predecessors.addBinding(source, edgeLabel)
    }
  }

  private def removeEdge(source: Node, edgeLabel: A, target: Node): Unit = {
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

  def allMinimalHittingSets(): collection.Set[collection.Set[A]] = {
    activeNodes() filter { _.isLeaf() } map { _.H }
  }

  def populate(): Unit = {
    expand(Set(rootNode))
  }

  @tailrec
  private def expand(nodes: collection.Set[Node]): Unit = {
    val nextLevel = Sets.newConcurrentHashSet[Node]().asScala
    nodes.foreachPar(node ⇒ {
      if (activeNodes().exists(activeNode ⇒ activeNode.isLeaf() && (activeNode.H strictSubsetOf node.H))) {
        node.label = new ClosedNode()
      } else {
        node.label =
          findHyperedge(hyperedge ⇒ (hyperedge intersect node.H).isEmpty)
            .map(hyperedge ⇒ { pruneWith(hyperedge); new HyperedgeNode(hyperedge) })
            .getOrElse(new LeafNode())
        node.hyperedge().foreach(hyperedge ⇒ {
          hyperedge.foreach(edgeLabel ⇒ {
            val successor =
              activeNodes().find(activeNode ⇒ activeNode.H equals (node.H + edgeLabel))
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

  private def pruneWith(hyperedge: collection.Set[A]): Unit = {
    activeNodes().foreachPar(activeNode ⇒ {
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
            certainlyToBeRemoved.foreach(node ⇒ removeNode(node))
            ignoreHyperedge(otherHyperedge)
          }
        }
        case default ⇒ {}
      }
    })
  }

}
