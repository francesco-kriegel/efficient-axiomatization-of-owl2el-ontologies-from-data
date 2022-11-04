package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{NodeID, OWLAnonymousIndividual, OWLClass, OWLClassExpression, OWLIndividual, OWLObjectProperty}
import uk.ac.manchester.cs.owl.owlapi.OWLAnonymousIndividualImpl

import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}
import java.text.SimpleDateFormat
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.io.StdIn.readLine
import scala.jdk.StreamConverters.*
import util.control.Breaks.*
//import collection.parallel.CollectionConverters.IterableIsParallelizable

object MainDeprecated {

  def main(args: Array[String]): Unit = {

    val startTime = System.currentTimeMillis()

    val manager = OWLManager.createOWLOntologyManager()

    //    for (n <- 20 to 16855) {
    //      val ontologyFile = new File("/Users/francesco/workspace/Data/ore2015_pool_sample/files/ore_ont_" + n + ".owl")
    //      if (ontologyFile.exists()) {
    //        val ontology = manager.loadOntologyFromOntologyDocument(ontologyFile)
    //        val individuals = (ontology.individualsInSignature().toScala(LazyList) concat ontology.anonymousIndividuals().toScala(LazyList)).toSet[OWLIndividual]
    //        println("ore_ont_" + n + ".owl   " + individuals.size + " individuals")
    //      }
    //    }

    val (ontologyFile, ont) = readInputFileName("Enter number of test ontology in the ORE2015 pool sample: ")
    val ontology = manager.loadOntologyFromOntologyDocument(ontologyFile)

    val variables = mutable.HashMap[OWLClassExpression, OWLAnonymousIndividual]()
//    def variableFor(classExpression: OWLClassExpression): OWLAnonymousIndividual = {
//      variables.getOrElseUpdate(classExpression, new OWLAnonymousIndividualImpl(NodeID.getNodeID()))
//    }
    def variableFor = variables.getOrElseUpdate(_, new OWLAnonymousIndividualImpl(NodeID.getNodeID()))

    def unfoldABoxAssertion(classExpression: OWLClassExpression, individual: OWLIndividual): Unit = {
      classExpression.conjunctSet().forEach({
        case owlClass @ Class(_) if !(owlClass equals OWLNothing) =>
          ontology.addAxiom(individual Type owlClass)
        case ObjectSomeValuesFrom(property @ ObjectProperty(_), filler) =>
          val successor = variableFor(filler)
          ontology.addAxiom(individual Fact (property, successor))
          unfoldABoxAssertion(filler, successor)
        case _ =>
          Console.err.println("Unsupported class expression: " + classExpression)
      })
    }

    ontology.aboxAxioms(Imports.INCLUDED).toScala(LazyList)
      .foreach({
        case ClassAssertion(_, classExpression, individual) =>
          unfoldABoxAssertion(classExpression, individual)
        case _ => {}
      })

    val graph = OntologyGraph(ontology)

    println(graph.nodes.size + " individuals")

    println("Computing pre-simulation...")

//    val simulation = ConcurrentReflexiveRelation[OWLIndividual]()
    val simulation = ReflexiveBitRelation[OWLIndividual]()
    val nodesPar = new scala.collection.parallel.mutable.ParArray[OWLIndividual](graph.nodes.toArray)

    class Counter() {
      var n = 0
      def tick(): Unit = {
        n = n + 1
        print("\r" + n)
      }
      def reset(): Unit = {
        println()
        n = 0
      }
    }
    val counter = Counter()

    for (y <- graph.nodes) {
      counter.tick()
      val yLabels = graph.labels(y).toSet
      val yProperties = graph.successors(y).map({ case (property, _) => property }).toSet
//      for (x <- nodesPar if !(x equals y)) {
      for (x <- graph.nodes if !(x equals y)) {
        if ((graph.labels(x) forall (yLabels contains _))
            && (graph.successors(x).map({ case (property, _) => property }) forall (yProperties contains _))) {
            simulation.add(x,y)
        }
      }
    }
    counter.reset()

    println("Computing initial mapping R(x,r)...")

    val R = new mutable.HashMap[(OWLIndividual, OWLObjectProperty), mutable.Set[OWLIndividual]] with mutable.MultiMap[(OWLIndividual, OWLObjectProperty), OWLIndividual]

    for (x <- nodesPar) {
      counter.tick()
      for (yy <- nodesPar) {
        for (property <- graph.successors(yy).map({ case (property, _) => property }).distinct) {
          if (!graph.successorsForRelation(yy, property).exists(simulation(x, _))) {
            (x, property).synchronized {
              R.addBinding((x, property), yy)
            }
          }
        }
      }
    }
//    for (yy <- nodesPar) {
//      counter.tick()
//      for ((r,y) <- graph.successors(yy)) {
//        for (x <- simulation.col(y)) {
//          (x, r).synchronized {
//            R.addBinding((x, r), yy)
//          }
//        }
//      }
//    }
    counter.reset()

    println("Computing simulation...")

    @tailrec
    def loop(): Unit = {
      counter.tick()
      val opt = R.keys.find({ case (x,r) => R.contains(x,r) && R(x,r).nonEmpty })
      if (opt.isDefined) {
        val (x, r) = opt.get
        for (xx <- graph.predecessorsForRelation(x, r)) {
          for (yy <- R(x, r)) {
            if (simulation(xx, yy)) {
              simulation.remove(xx, yy)
              for ((rr, yyy) <- graph.predecessors(yy)) {
                if (graph.successorsForRelation(yyy, rr).exists(simulation(xx, _))) {
                  R.addBinding((xx,rr), yyy)
                }
              }
            }
          }
        }
        R.remove(x, r)
        loop()
      }
    }

    loop()
    counter.reset()

    println("Computing equivalence classes...")

    val equivalenceClasses = mutable.ListBuffer[mutable.ArrayBuffer[OWLIndividual]]()
    val remainingNodes = mutable.HashSet[OWLIndividual]()
    remainingNodes.addAll(graph.nodes)
    while (remainingNodes.nonEmpty) {
      counter.tick()
      val representative = remainingNodes.head
      val equivalenceClass = simulation.rowAsArrayBuffer(representative).filter(simulation.colAsSet(representative)(_))
      equivalenceClasses.addOne(equivalenceClass)
//      remainingNodes.filterInPlace(!equivalenceClass(_))
      equivalenceClass.foreach(remainingNodes.remove(_))
    }
    counter.reset()

    println(equivalenceClasses.size + " equivalence classes")

    println("Computing reduction...")

    val reduction = ListGraph[Int, OWLClass, OWLObjectProperty]()
//    equivalenceClasses.map(_.hashCode).foreach(reduction.addNode(_))
    val index = mutable.HashMap[mutable.ArrayBuffer[OWLIndividual], Int]()
    var i = 1
    for (xs <- equivalenceClasses) {
      index.put(xs, i)
      i += 1
    }
    val n = i - 1
//    val ecPar = new scala.collection.parallel.mutable.ParArray[Set[OWLIndividual]](equivalenceClasses.toArray)
    for (xs <- equivalenceClasses) {
      counter.tick()
      reduction.addNode(index(xs))
      graph.labels(xs.head).foreach(reduction.addLabel(index(xs), _))
      for (ys <- equivalenceClasses) {
        graph.successors(xs.head).map({ case (r, _) => r }).distinct.foreach(r => {
          if (graph.successorsForRelation(xs.head, r).exists(simulation(ys.head, _))
            && graph.successorsForRelation(xs.head, r).forall(z => !simulation(ys.head, z) || simulation(z, ys.head))) {
            reduction.addEdge(index(xs), r, index(ys))
          }
        })
      }
    }
    counter.reset()

//    val reductionPar = new scala.collection.parallel.mutable.ParArray[OWLIndividual](reduction.nodes.toArray)
//    val reductionNodesPar = new scala.collection.parallel.mutable.ParHashSet[Int]()
//    reductionNodesPar.addAll(reduction.nodes)

    val powering = ListGraph[BitSet, OWLClass, OWLObjectProperty]()
    def extendPowering(xs: BitSet): collection.Set[BitSet] = {
      val delta = mutable.HashSet[BitSet]()

      @tailrec
      def extend(current: IterableOnce[BitSet]): Unit = {
//        println("ext...")
        val next = mutable.HashSet[BitSet]()
        for (xs <- current) {
          if (!powering.nodes.contains(xs)) {
            powering.nodes.addOne(xs)
            delta.add(xs)
            val labels = xs.unsorted.tail.map(reduction.labels(_).toSet).foldLeft(reduction.labels(xs.head).toSet)(_ intersect _)
            //        powering.labels(xs).addAll(labels)
            powering.addLabels(xs, labels)
            val relations = reduction.successors(xs.head).map({ case (r, _) => r }).toSet
            relations.foreach(r => {
              val hypergraph: collection.Set[collection.Set[Int]] = xs.unsorted.map(x => BitSet.fromSpecific(reduction.successorsForRelation(x, r)))
              HSdag.allMinimalHittingSets(hypergraph).foreach(mhs => {
                val ys = BitSet.fromSpecific(mhs)
                powering.addEdge(xs, r, ys)
//                extendPowering(ys)
//                if (!powering.nodes.contains(ys))
                  next.addOne(ys)
              })
            })
          }
        }
        if (next.nonEmpty)
          extend(next)
      }
      extend(Set(xs))
//      println("Added " + delta.size + " elements to the powering")
      delta
    }

//    val poweringSimulation = ConcurrentRelation[mutable.BitSet, Int]()
    val poweringSimulation = BitSetToIntRelation()

//    def closure(_xs: BitSet): BitSet = {
//      var __xs = mutable.BitSet()
//      __xs.addAll(_xs)
////      println("Computing a closure...")
////      counter.tick()
//      poweringSimulation.rows.foreach(ys => {
//        if (ys subsetOf __xs)
//          __xs.addAll(poweringSimulation.row(ys))
//      })
//      val xs = __xs.toImmutable
    def closure(xs: BitSet): BitSet = {
      val delta = extendPowering(xs)
      if (delta.nonEmpty) {
//        counter.tick()
//        print("\r      #####")
//        println("Computing the border...")
        val border = powering.nodes.filter(!delta(_)).filter(u => powering.predecessors(u).exists((_, v) => delta(v))).toSet
        for (y <- reduction.nodes) {
          val yLabels = reduction.labels(y).toSet
          val yProperties = reduction.successors(y).map({ case (property, _) => property }).toSet
          for (ds <- delta) {
            if (ds(y)) {
//              poweringSimulation.add(ds, y)
            } else if ((powering.labels(ds) forall (yLabels contains _))
              && (powering.successors(ds).map({ case (property, _) => property }) forall (yProperties contains _))) {
              poweringSimulation.add(ds, y)
            }
          }
        }

//        println("Computing the mapping R(x,r)...")
//        val powR = new collection.concurrent.TrieMap[(BitSet, OWLObjectProperty), mutable.BitSet] //with mutable.MultiMap[(BitSet, OWLObjectProperty), Int]
        val powR = new mutable.HashMap[(BitSet, OWLObjectProperty), mutable.BitSet]

        for (x <- delta union border) {
//          counter.tick()
          for (yy <- reduction.nodes) {
            for (property <- reduction.successors(yy).map({ case (property, _) => property }).distinct) {
              if (!reduction.successorsForRelation(yy, property).exists(poweringSimulation(x, _))) {
//                (x, property).synchronized {
                  // powR.addBinding((x, property), yy)
                  powR.getOrElseUpdate((x, property), mutable.BitSet()).addOne(yy)
//                }
              }
            }
          }
        }

//        println("Updating the simulation...")
        @tailrec
        def powLoop(): Unit = {
//          counter.tick()
//          val opt = powR.keys.find({ case (x, r) => powR.contains(x, r) && powR(x, r).nonEmpty })
//          val opt = powR.keys.find({ case (x, r) => powR(x, r).nonEmpty })
//          if (opt.isDefined) {
//            val (x, r) = opt.get
          if (powR.keySet.nonEmpty) {
            val (x, r) = powR.keySet.head
            for (xx <- powering.predecessorsForRelation(x, r)) {
              for (yy <- powR(x, r)) {
                if (!xx(yy) && poweringSimulation(xx, yy)) {
                  poweringSimulation.remove(xx, yy)
                  for ((rr, yyy) <- reduction.predecessors(yy)) {
                    if (reduction.successorsForRelation(yyy, rr).exists(poweringSimulation(xx, _))) {
//                      powR.addBinding((xx, rr), yyy)
                      powR.getOrElseUpdate((xx, rr), mutable.BitSet()).addOne(yyy)
                    }
                  }
                }
              }
            }
            powR.remove((x, r))
            powLoop()
          }
        }

        powLoop()

      }
      powering.clear()
//      println("Closure computation done.")
      poweringSimulation.row(xs)
    }

    println("Computing closures...")

    val closures = mutable.HashSet[BitSet]()

    def subsetOfUpTo(xs: BitSet, j: Int, ys: BitSet): Boolean = {
      var result = true
      val it = xs.iterator
      breakable {
        while (it.hasNext) {
          val x = it.next()
          if (x < j) {
            if (!ys(x)) {
              result = false
              break
            }
          } else {
            break
          }
        }
      }
      result
    }

//    def equalsUpTo(xs: BitSet, j: Int, ys: BitSet): Boolean = {
//      var result = true
//      val xIt = xs.iterator
//      val yIt = ys.iterator
//      breakable {
//
//        val xHasNext = xIt.hasNext
//        val yHasNext = yIt.hasNext
//        if (xHasNext != yHasNext) {
//          break
//        }
//      }
//      result
//    }

////    @tailrec
//    def FCbO(xs: BitSet, i: Int, ns: Int => BitSet): Unit = {
//      val queue = mutable.Queue[(BitSet, Int)]()
//      val ms = mutable.HashMap[Int, BitSet]()
//      for (j <- i to n) {
//        ms.put(j, ns(j))
//        if (!xs(j)) {
//          if (subsetOfUpTo(ns(j), j, xs)) {
//            val ys = closure(xs + j)
//            if (subsetOfUpTo(xs, j, ys) && subsetOfUpTo(ys, j, xs))
//              queue.enqueue((ys, j))
////            if (xs.forall(x => x >= j || ys(x)) && ys.forall(y => y >= j || xs(y)))
////              queue.enqueue((ys,j))
//            else ms.put(j, ys)
//          }
//        }
//      }
//      while (queue.nonEmpty) {
////        counter.tick()
//        val (ys, j) = queue.dequeue()
//        closures.addOne(ys)
//        FCbO(ys, j+1, ms)
//      }
//    }
//
//    FCbO(BitSet.empty, 1, _ => BitSet.empty)

    val outerDeque = mutable.ArrayDeque[(BitSet, Int, Int => BitSet)]()
    outerDeque.prepend((BitSet.empty, 1, (_: Int) => BitSet.empty))

    while (outerDeque.nonEmpty) {
//      print("\rqueue size: " + outerDeque.size)
      val (xs, i, ns) = outerDeque.removeHead()
      closures.addOne(xs)
      val innerQueue = mutable.Queue[(BitSet, Int)]()
      val ms = mutable.HashMap[Int, BitSet]()
      for (j <- i to n) {
//        print("\rqueue size: " + outerDeque.size + "  ---  " + j)
        ms.put(j, ns(j))
        if (!xs(j)) {
          if (subsetOfUpTo(ns(j), j, xs)) {
            val ys = measureExecutionTime({ closure(xs + j) }, "\router queue size: " + outerDeque.size + "  ---  inner queue position: " + j + "  ---  closure computation took ")
//            val ys = closure(xs + j)
            if (subsetOfUpTo(xs, j, ys) && subsetOfUpTo(ys, j, xs))
              innerQueue.enqueue((ys, j))
            else ms.put(j, ys)
          }
        }
      }
//      while (innerQueue.nonEmpty) {
//        val (ys, j) = innerQueue.dequeue()
//        outerDeque.prepend((ys, j + 1, ms))
//      }
      outerDeque.prependAll(innerQueue.map((ys, j) => (ys, j + 1, ms)))
    }

    counter.reset()

    println(closures.size + " closures")

    val objects = reduction.nodes
    val attributes = OWLNothing :: (ontology.classesInSignature().toScala(LazyList).filterNot(_ equals OWLThing).filterNot(_ equals OWLNothing)
      ++ ontology.objectPropertiesInSignature().toScala(LazyList).flatMap(r => closures.map(c => (r, c)))).toList
    def incidence(g: Int, m: AnyRef): Boolean = {
      m match
        case OWLNothing => false
        case A @ Class(_) => reduction.labels(g).contains(A)
        case (r @ ObjectProperty(_), c: BitSet) => reduction.successorsForRelation(g, r).exists(c(_))
//        case _ => false
    }
    val occupiedAttributes = attributes.filter(m => (m equals OWLNothing) || objects.exists(g => incidence(g, m)))

    def toString(m: AnyRef): String = {
      m match
        case OWLNothing => OWLNothing.toString
        case A @ Class(_) => A.toString
        case (r @ ObjectProperty(_), c: BitSet) => "ObjectSomeValuesFrom(" + r + "," + c.mkString("MMSC(", ",", ")") + ")"
    }

    val basepath = "/Users/francesco/workspace/Java_Scala/efficient-axiomatization-of-owl2el-ontologies-from-data/"
    val cxtFile = new File(basepath + ont + ".cxt")
    val writer = new BufferedWriter(new FileWriter(cxtFile))
    writer.write("B\n")
    writer.write("\n")
    writer.write(objects.size + "\n")
    writer.write(occupiedAttributes.size + "\n")
    writer.write("\n")
    objects.foreach(g => writer.write(g.toString + "\n"))
    occupiedAttributes.foreach(m => writer.write(toString(m) + "\n"))
    objects.foreach(g => {
      occupiedAttributes.foreach(m => {
        if (incidence(g, m)) writer.write("X")
        else writer.write(".")
      })
      writer.write("\n")
    })
    writer.write("\n\n\n")
    attributes.foreach(m => if (!occupiedAttributes.contains(m)) writer.write(toString(m) + "\n"))
    writer.close()

    println("Induced context written to " + cxtFile)

    println("Running LinCbO...")
    println(sys.process.Process(basepath + "LinCbO/fcai/CanonicalBasis/fcai " + basepath + ont + ".cxt " + basepath + ont + ".canbase 1").!!)

    val reader = new BufferedReader(new FileReader(new File(basepath + ont + ".canbase-myCboObLinNoPruning")))
//    reader.lines().forEach(println(_))
    reader.lines.forEach(_ => counter.tick())
    reader.close()

    counter.reset()

    val computationTime = System.currentTimeMillis() - startTime
    println(formatTime(computationTime))

  }

  def measureExecutionTime[T](code: => T, text: String): T = {
    val start = System.currentTimeMillis()
    val result = code
    val duration = System.currentTimeMillis() - start
    print(text + formatTime(duration))
    result
  }

  def formatTime(t: Long): String = {
    var x = t
    val SSS = x % 1000
    x /= 1000
    val ss = x % 60
    x /= 60
    val mm = x % 60
    x /= 60
    val HH = x
    HH + "h " + (if (mm < 10) "0" + mm else mm) + "min " + (if (ss < 10) "0" + ss else ss) + "s " + (if (SSS < 100) "0" + (if (SSS < 10) "0" + SSS else SSS) else SSS) + "ms"
  }

  @tailrec
  private def readInputFileName(message: String): (File, String) = {
    print(message)
    val ont = "ore_ont_" + readLine()
    val file = new File("/Users/francesco/workspace/Data/ore2015_pool_sample/files/" + ont + ".owl")
    if (file.exists) {
      (file, ont)
    } else {
      System.err.println("The file could not be found.")
      println()
      readInputFileName(message)
    }
  }

}
