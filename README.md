## Efficient Axiomatization of OWL 2 EL Ontologies from Data by means of Formal Concept Analysis

This is a prototypical implementation of the approach developed and described in the conference article:
> Francesco Kriegel: Efficient Axiomatization of OWL 2 EL Ontologies from Data by means of Formal Concept Analysis. In *Proceedings of the 38th Annual AAAI Conference on Artificial Intelligence (AAAI 2024), February 20–27, 2024, Vancouver, Canada,* 2024. To appear.

The extended version with all technical details and proofs will be available from [doi:10.25368/2023.214](https://doi.org/10.25368/2023.214) soon.

---
### Usage

This prototype is developed in the programming language Scala 3.2.1.  As JDK we used Oracle GraalVM EE 22.3.0 (Java 19.0.1).

To compile and run the code, run the following two commands:
1. `sbt assembly`
2. `java -jar target/scala-3.2.1/efficient-axiomatization-of-owl2el-ontologies-from-data-assembly-0.2.1-SNAPSHOT.jar`
You can specify the JDK to be used with the option `--java-home` of `sbt` in the first command, and by replacing `java` with a fully qualified path in the second command.

Note that this prototype has so far only been used for the empirical evaluation in the above mentioned research paper.  In the current version, it only reads input datasets from the folder `ore2015_pool_sample` and therein expects the corpus from the ORE 2015 Reasoner Competition ([doi:10.5281/zenodo.18578](https://doi.org/10.5281/zenodo.18578)).  See also the Bash script `runExperiments.sh`, which controls the execution of the prototype on the test datasets.  It would be easy to adapt the code to support other input datasets and to actually write the axiomatized ontology to disk.  If you cannot do this on your own, please contact me.
