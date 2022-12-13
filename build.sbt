
ThisBuild / organization := "de.tu_dresden.inf.lat"
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "efficient-axiomatization-of-owl2el-ontologies-from-data",
    idePackagePrefix := Some("de.tu_dresden.inf.lat")
  )

resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies += "net.sourceforge.owlapi" % "owlapi-distribution" % "5.1.20"
libraryDependencies += "org.phenoscape" %% "scowl-owlapi5" % "1.4.1"
libraryDependencies += "org.semanticweb.elk" % "elk-owlapi5" % "0.5.0-SNAPSHOT"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.slf4j" % "slf4j-nop" % "2.0.4"
libraryDependencies += "org.scala-lang" % "scala-library" % "2.13.11-bin-SNAPSHOT"
//libraryDependencies += "com.netflix.hollow" % "hollow" % "7.1.7"


//ThisBuild / assemblyShadeRules :=
//  Seq("scala.collection.immutable.BitSet",
//      "scala.collection.immutable.BitSet$",
//      "scala.collection.immutable.BitSet$BitSet1",
//      "scala.collection.immutable.BitSet$BitSet2",
//      "scala.collection.immutable.BitSet$BitSetN",
//      "scala.collection.immutable.BitSet$SerializationProxy"
//  ) map { pattern =>
//    ShadeRule.zap(pattern).inAll//.inLibrary("org.scala-lang" % "scala-library" % "2.13.10")
//  }

ThisBuild / assemblyMergeStrategy  := {
  case PathList("module-info.class") => MergeStrategy.discard
  case x if x.endsWith("/module-info.class") => MergeStrategy.discard
//  case x if x.contains("scala/collection/immutable/BitSet") => println(x); MergeStrategy.preferProject
//  case x if x.contains("scala/collection/immutable/BitSet") =>
//    import sbtassembly.Assembly.{Project, Library}
//    CustomMergeStrategy.rename {
//      case dependency@(_: Project) => dependency.target
//      case dependency@(_: Library) => dependency.target + "_from_library"
//    }
  case x =>
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}

Compile / mainClass := Some("de.tu_dresden.inf.lat.axiomatization.Main")