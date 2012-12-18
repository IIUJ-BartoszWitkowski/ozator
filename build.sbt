name := "ozer"

version := "0.0.1"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation")

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "1.8" % "test",
	"junit" % "junit" % "4.10" % "test",
	"org.ini4j" % "ini4j" % "0.5.2",
	"commons-lang" % "commons-lang" % "2.6")

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

