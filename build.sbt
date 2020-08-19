name := "MUD"
version := "1.0"
scalaVersion := "2.12.12"

mainClass in (Compile, packageBin) := Some("mud.Main")

mainClass in (Compile, run) := Some("mud.Main")

addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.3")
bloopMainClass in (Compile, run) := Some("Main.App")

libraryDependencies ++= Seq(
	"com.typesafe.akka" %% "akka-actor" % "2.5.8",
	"com.typesafe.akka" %% "akka-testkit" % "2.5.8" % Test,
	"org.scala-lang.modules" %% "scala-xml" % "1.0.6",
	"com.novocode" % "junit-interface" % "0.11" % Test,
	"org.scalactic" %% "scalactic" % "3.0.4",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
   
)

