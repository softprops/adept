object Build extends sbt.Build {
  import sbt._
  import Keys._
  lazy val core = Project("adept-core", file("."))
  lazy val script = Project("adept-script", file("script")).dependsOn(core)
}
