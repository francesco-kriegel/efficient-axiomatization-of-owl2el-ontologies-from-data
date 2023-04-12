
ThisBuild / organization := "de.tu_dresden.inf.lat"
ThisBuild / version := "0.2.1-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "efficient-axiomatization-of-owl2el-ontologies-from-data",
    idePackagePrefix := Some("de.tu_dresden.inf.lat")
  )

resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies += "org.scala-lang" %% "scala3-library" % "3.2.1"
libraryDependencies += "org.scala-lang" % "scala-library" % "2.13.11-bin-SNAPSHOT"
//libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "net.sourceforge.owlapi" % "owlapi-distribution" % "5.1.20"
libraryDependencies += "org.phenoscape" %% "scowl-owlapi5" % "1.4.1"
libraryDependencies += "org.semanticweb.elk" % "elk-owlapi5" % "0.5.0-SNAPSHOT"
libraryDependencies += "org.slf4j" % "slf4j-nop" % "2.0.4"

ThisBuild / assemblyMergeStrategy  := {
  case PathList("module-info.class") => MergeStrategy.discard
  case x if x.endsWith("/module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}

Compile / mainClass := Some("de.tu_dresden.inf.lat.axiomatization.Main")