organization := "me.lessis"

name := "adept-script"

version := "0.1.0-SNAPSHOT"

resolvers += Classpaths.typesafeResolver

libraryDependencies <+= (sbtVersion)(
  "org.scala-sbt" %
   "launcher-interface" %
    _ % "provided")
