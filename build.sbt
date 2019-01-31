name := "rats"
version := "0.1.0"
organization := "mq"

// Scala compiler settings
scalaVersion := "2.12.8"
scalacOptions ++= Seq ("-deprecation", "-feature", "-unchecked")

// Interactive settings
logLevel := Level.Info
shellPrompt in ThisBuild := {
    state =>
        Project.extract(state).currentRef.project + " " + version.value +
            " " + scalaVersion.value + "> "
}

resolvers += Resolver.url ("scalasbt",
  new URL ("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases")
) (Resolver.ivyStylePatterns)
resolvers += Resolver.url ("Artima Maven Repository",
  new URL ("http://repo.artima.com/releases")
) (Resolver.ivyStylePatterns)
//resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"


// Fork the runs and connect sbt's input and output to the forked process so
// that we are immune to version clashes with the JLine library used by sbt

fork in run := true

connectInput in run := true

outputStrategy in run := Some (StdoutOutput)

// Dependencies

libraryDependencies ++=
    Seq (
        "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.2.0",
        "org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.2.0",
	      "org.scala-graph" %% "graph-constrained" % "1.12.3",
        "org.jopendocument" % "jOpenDocument" % "1.3"
    )

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

dependencyOverrides += "jline" % "jline" % "2.15-SNAPSHOT"
resolvers ++= Seq (
    Resolver.sonatypeRepo ("releases"),
    Resolver.sonatypeRepo ("snapshots")
)

// Rats! setup

ratsScalaRepetitionType := Some (VectorType)

ratsUseScalaOptions := true

ratsUseScalaPositions := true

ratsDefineASTClasses := true

ratsDefinePrettyPrinter := true

ratsUseKiama := 2
