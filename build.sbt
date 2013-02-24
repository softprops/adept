organization := "me.lessis"

name := "adept"

version := "0.1.0-SNAPSHOT"

resolvers += Classpaths.typesafeResolver

libraryDependencies <+= (sbtVersion)(
  "org.scala-sbt" %
   "launcher-interface" %
    _ % "provided")

libraryDependencies += "me.lessis" %% "semverfi" % "0.1.1"
                                                                                
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "2.2.0.201212191850-r"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.1.0"