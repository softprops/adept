package adept.cmds

import adept.Manager

object List extends Cmd {

  val OrgAndName = """(.+):(.+)""".r

  def apply(args: Array[String]) =
    args match {
      case Array(name) =>
        name match {
          case OrgAndName(org, name) =>
            Manager.modules.byOrgAndName(org, name).map(show)
          case name =>
            if (name contains(".")) Manager.modules.byOrganization(name).map(show)
            else Manager.modules.byName(name).map(show)
        }
        Right("")
      case _ =>
        Manager.modules.list.map(show)
        Right("")
    }

  def show(m: (String, Seq[String], String)) =
    m match {
      case (name, versions, repo) =>
        println("- %s" format name)
        versions.foreach(v => println("  %s" format v))
    }
}
