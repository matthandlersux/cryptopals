name := "cryptopals"

version := "1.0"

scalaVersion := "2.11.0"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "commons-codec" % "commons-codec" % "1.6",
  "com.typesafe.play" %% "play-json" % "2.3.4"
)