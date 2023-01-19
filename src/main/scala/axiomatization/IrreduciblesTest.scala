package de.tu_dresden.inf.lat
package axiomatization


@Deprecated(forRemoval = true)
object IrreduciblesTest {

  def main(args: Array[String]): Unit = {

    given logger: Logger = NoLogger()

    val ReducedOreOntologyOWL = raw"ore_ont_(\d+)_reduced.owl".r
    val OreOntologyOWL = raw"ore_ont_(\d+).owl".r
    val OreOntologyCSV = raw"ore_ont_(\d+).csv".r
    val Success = raw";(None|Fast|Canonical);Success;".r.unanchored

    val subs = scala.collection.immutable.Set("dl/classification", "dl/consistency", "dl/instantiation", "el/classification", "el/consistency", "el/instantiation")
      .map(sub =>  {
        val fileorder = java.io.File("/Users/francesco/workspace/Java_Scala/efficient-axiomatization-of-owl2el-ontologies-from-data/ore2015_pool_sample/" + sub + "/fileorder.txt")
        val source = scala.io.Source.fromFile(fileorder)
        val onts = source.getLines().map(OreOntologyOWL.findFirstMatchIn(_).get.group(1).toInt).toSet
        source.close()
        sub -> onts
      })
      .toMap

    def getSubs(ont: Int): scala.collection.Set[String] = {
      subs.keySet.filter(subs(_) contains ont)
    }

    val experimentFolder = java.io.File("/Users/francesco/workspace/Java_Scala/efficient-axiomatization-of-owl2el-ontologies-from-data/ore2015_pool_sample_experiments_russell_performance")
    val reductionFolder = java.io.File(experimentFolder, "files")
    val resultsFolder = java.io.File(experimentFolder, "results")

    val irreduciblesStatistics = scala.collection.mutable.HashMap[Int, (Int, Int, Float)]()

    val sizes =
      java.io.File(experimentFolder, "statistics")
        .listFiles()
        .filter(file => OreOntologyCSV.matches(file.getName))
        .map(file => {
          val fileSource = scala.io.Source.fromFile(file)
          val size = fileSource.getLines().next().split(';').apply(1).toInt
          fileSource.close()
          val ont = OreOntologyCSV.findFirstMatchIn(file.getName).get.group(1).toInt
          ont -> size
        })
        .toMap

    val reductionsWithoutAxiomatization =
      reductionFolder.listFiles().filter(file => {
        val firstMatch = ReducedOreOntologyOWL.findFirstMatchIn(file.getName)
        firstMatch.isDefined && {
          val ont = firstMatch.get.group(1).toInt
          val resultFile = java.io.File(resultsFolder, s"ore_ont_$ont.csv")
          resultFile.exists() && {
            val resultSource = scala.io.Source.fromFile(resultFile)
            val tryComputeIrreducibles = !resultSource.getLines().exists(Success.matches)
            resultSource.close()
            tryComputeIrreducibles
          }
        }
      }).sortWith((file1, file2) => {
        val ont1 = ReducedOreOntologyOWL.findFirstMatchIn(file1.getName).get.group(1).toInt
        val ont2 = ReducedOreOntologyOWL.findFirstMatchIn(file2.getName).get.group(1).toInt
        sizes(ont1) < sizes(ont2)
      })

    println(s"${reductionsWithoutAxiomatization.size} reductions without axiomatization")
//    reductionsWithoutAxiomatization.foreach(file => {
//      val ont = ReducedOreOntologyOWL.findFirstMatchIn(file.getName).get.group(1).toInt
//      println(ont -> sizes(ont))
//    })

    // TODO: check from 6
    reductionsWithoutAxiomatization.drop(0).foreach(file => {

      val ont = ReducedOreOntologyOWL.findFirstMatchIn(file.getName).get.group(1).toInt
      val ontology = org.semanticweb.owlapi.apibinding.OWLManager.createOWLOntologyManager().loadOntologyFromOntologyDocument(file)
      val graph = BitGraph.fromOntology(ontology)
      val size = graph.nodes().size

      println(f"Ontology: ${file.getName}")
      println(f"Objects: $size")

      if (false) {

        val simulation = GraphSimulator.computeMaximalSimulation(graph, graph)

        val strictlyAboveArray = new scala.Array[scala.collection.mutable.BitSet](size)
        val doubleStrictlyAboveArray = new scala.Array[scala.collection.mutable.BitSet](size)
        val upperNeighborsArray = new scala.Array[scala.collection.mutable.BitSet](size)

        def arrayGetOrUpdate[T](array: scala.Array[T], index: Int, update: Int => T): T =
          if (array(index) == null)
            array(index) = update(index)
          array(index)

        def strictlyAbove(node: Int): scala.collection.mutable.BitSet =
          arrayGetOrUpdate(strictlyAboveArray, node, n => {
            simulation.row(n).viewAsMutableBitSet diff simulation.col(n).viewAsMutableBitSet
          })

        def doubleStrictlyAbove(node: Int): scala.collection.mutable.BitSet =
          arrayGetOrUpdate(doubleStrictlyAboveArray, node, n => {
            strictlyAbove(n).flatMap(strictlyAbove)
          })

        def upperNeighbors(node: Int): scala.collection.mutable.BitSet =
          arrayGetOrUpdate(upperNeighborsArray, node, n => {
            strictlyAbove(n) diff doubleStrictlyAbove(n)
          })

  //      val clop1 = PoweringClosureOperator(graph)
        val clop2 = PoweringClosureOperator(graph, None, Some(64), false)
//        val clop2a = PoweringClosureOperator2a(graph)
  //      val clop3 = PoweringClosureOperator3(graph)

  //      var totalDuration2 = 0l
  //      var totalDuration2a = 0l

        def isIrreducible(node: Int): Boolean = {
          val uppers = upperNeighbors(node)
          //            println("   " + uppers.size + "upper neighbors")
  //        (uppers.size < 2) || !clop(uppers).contains(node)
          (uppers.size < 2) || {
  //          if (uppers.size > 9) print("   For a set with " + uppers.size + " elements, ")
  //          val closure1 = Util.writeExecutionTime(clop1(uppers), duration => println("closure computation 1 took " + Util.formatTime(duration)))
            val (closure2, duration2) = Util.measureExecutionTime(clop2(uppers))
  //          val (closure2a, duration2a) = Util.measureExecutionTime(clop2a(uppers))
  //          totalDuration2 += duration2
  //          totalDuration2a += duration2a
  //          if (uppers.size > 9) {
  //            val ratio = (duration2.toFloat / duration2a.toFloat) * 100f
  //            val totalRatio = (totalDuration2.toFloat / totalDuration2a.toFloat) * 100f
  //            println(f"the ratio is $ratio%1.2f%% (total ratio: $totalRatio%1.2f%%)")
  //          }
  //          if (!(closure2 equals closure2a))
  //            System.err.println(node)
  //            System.err.println(uppers)
  //            System.err.println(closure2)
  //            System.err.println(closure2a)
  //            throw RuntimeException()
            !closure2.contains(node)
          }
        }

        //        val irreducibles = graph.nodes().par.filter(isIrreducible)
        var i = 0
        var j = 0
        val irreducibles = graph.nodes().filter(node => {
          j += 1
          val result = isIrreducible(node)
          if (result)
            i += 1
            val r = (i.toFloat / j.toFloat) * 100f
            print(f"\rIrreducibles: $i / $j ($r%1.2f%%)")
          result
        })
        println()

        val ratio = (irreducibles.size.toFloat / size.toFloat) * 100f
        //            println(f"Irreducibles: ${irreducibles.size} / $size ($ratio%1.2f%%)")
        println()

        irreduciblesStatistics.put(ont, (i, j, ratio))

//      println("Computing powering... ")

//      val powering = BitGraph[org.semanticweb.owlapi.model.OWLClass, org.semanticweb.owlapi.model.OWLObjectProperty]()
//
//      var i = 0
//
//      val index = scala.collection.mutable.ArrayBuffer[scala.collection.BitSet]()
//      val table = scala.collection.mutable.HashMap[scala.collection.BitSet, Int]()
//
//      def addToPowering(xs: scala.collection.BitSet): Int = {
////        index(i) = xs
//        index.addOne(xs)
//        table(xs) = i
//        powering.nodes().addOne(i)
//        i += 1
//        i
//      }
//
//      @scala.annotation.tailrec
//      def extendPowering(current: IterableOnce[scala.collection.BitSet]): Unit = {
//        val next = scala.collection.mutable.HashSet[scala.collection.BitSet]()
//        for (xs <- current) {
//          if (!table.contains(xs) && xs.nonEmpty) {
//            print(s"\rComputing powering... $i objects   Current node size: ${xs.size}")
//            val j = addToPowering(xs)
//            val labels = xs.unsorted.tail.map(graph.labels(_)).foldLeft(graph.labels(xs.head))(_ intersect _)
//            powering.addLabels(j, labels)
////            val relations = graph.successorRelations(xs.head)
//            val relations = xs.unsorted.tail.foldLeft(graph.successorRelations(xs.head))(_ intersect graph.successorRelations(_))
//            relations.foreach(r => {
//              //            val hypergraph: collection.Set[collection.Set[Int]] = xs.unsorted.map(x => BitSet.fromSpecific(reduction.successorsForRelation(x, r)))
//              val hypergraph: collection.Set[collection.Set[Int]] = xs.unsorted.map(x => graph.successorsForRelation(x, r))
//                HSdag.allMinimalHittingSets(hypergraph).foreach(mhs => {
//  //                val ys = scala.collection.immutable.BitSet.fromSpecific(mhs)
//                  val max = mhs.filter(x => mhs.forall(y => !simulation(x, y)))
//                  val ys = scala.collection.immutable.BitSet.fromSpecific(max)
//                  if (!table.contains(ys) && ys.nonEmpty)
//                    addToPowering(ys)
//                  powering.addEdge(j, r, table(ys))
//                  next.addOne(ys)
//                  ()
//                })
//            })
//          }
//        }
//        if (next.nonEmpty)
//          extendPowering(next)
//      }
//
////      val initialNodes = graph.nodes().unsorted.map(upperNeighbors).filter(_.nonEmpty)
////      println(s"${initialNodes.size} initial nodes")
////      initialNodes.foreach(inode => println(inode.size))
////      extendPowering(initialNodes)
//      extendPowering(graph.nodes().unsorted.map(upperNeighbors).filter(_.size > 1))
//
//      println()
//
//      println("Reducing powering...")
//
//      val (redPow, _, _) = Interpretation.reductionOf(powering)
//
//      println(s"${redPow.nodes().size} / ${graph.nodes().size}")
//      println()

      }
      ()
    })

    irreduciblesStatistics.toSeq.sortWith(_._2._2 < _._2._2).foreach {
      case (ont, (i, j, r)) =>
        val subs = getSubs(ont)
        val subsString = if subs.isEmpty then "" else "in " + subs.mkString(", ")
        println(f"ore_ont_$ont: $i / $j ($r%1.2f%%) $subsString")
    }

    ()

  }

}
