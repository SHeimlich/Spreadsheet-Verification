name := "rats"

version := "0.1.0"

organization := "mq"

//export TERM=xterm-color

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
   //     "org.bitbucket.inkytonik.sbt-rats" %% "sbt-rats" % "2.5.0",
	      "org.scala-graph" %% "graph-constrained" % "1.12.3",
        "org.jopendocument" % "jOpenDocument" % "1.3"
    )

dependencyOverrides += "jline" % "jline" % "2.15-SNAPSHOT"
resolvers ++= Seq (
    Resolver.sonatypeRepo ("releases"),
    Resolver.sonatypeRepo ("snapshots")
)

// Rats! setup

//sbtRatsSettings

ratsScalaRepetitionType := Some (VectorType)

ratsUseScalaOptions := true

ratsUseScalaPositions := true

ratsDefineASTClasses := true

ratsDefinePrettyPrinter := true

ratsUseKiama := 2
