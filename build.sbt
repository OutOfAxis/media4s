name := "media4s"

organization := "org.matthicks"

version := "1.0.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:experimental.macros",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard")

libraryDependencies += "org.powerscala" %% "powerscala-core" % "2.0.2"

libraryDependencies += "org.powerscala" %% "powerscala-concurrent" % "2.0.2"

libraryDependencies += "com.outr.scribe" %% "scribe" % "1.2.5"

libraryDependencies += "org.im4java" % "im4java" % "1.4.0"

libraryDependencies += "org.apache.xmlgraphics" % "batik-transcoder" % "1.8"

libraryDependencies += "org.apache.xmlgraphics" % "batik-util" % "1.8"

libraryDependencies += "org.apache.xmlgraphics" % "batik-ext" % "1.8"

libraryDependencies += "org.apache.xmlgraphics" % "batik-bridge" % "1.8"

libraryDependencies += "org.apache.xmlgraphics" % "batik-dom" % "1.8"

libraryDependencies += "org.apache.xmlgraphics" % "batik-codec" % "1.8"

libraryDependencies += "org.apache.xmlgraphics" % "batik-xml" % "1.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

testOptions in Test += Tests.Argument("-oDF")
