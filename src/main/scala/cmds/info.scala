package adept.cmds

import adept.{ Artifact, Manager, Module }

object Info extends Cmd {
  def apply(args: Array[String]) = 
    args match {
      case Array(name) =>
        Manager.modules.info(name).right.map(show)
      case Array(name, version) =>
        Manager.modules.info(name, Some(version)).right.map(show)
    }

  def show(m: Module) =
    """
    |%s / %s @ %s
    |dependencies
    |%s
    |artifacts
    |%s
    |""".stripMargin.format(m.organization, m.name, m.version,
                           if (m.dependencies.isEmpty) "(none)"
                           else m.dependencies.map(dep).mkString("\n"),
                           if (m.artifacts.isEmpty) "(none)"
                           else m.artifacts.map(artifact).mkString("\n"))
  def dep(m: Module) =
    "  %s / %s @ %s" format(m.organization, m.name, m.version)

  def artifact(a: Artifact) =
    "  %s" format(a.hash)
}
