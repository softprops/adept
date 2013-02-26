package adept.cmds

import adept.{ Manager, Module }

object Add extends Cmd {
  def apply(args: Array[String]) =
    args match {
      case Array(json) =>
        Manager.repos.list.headOption.map { repo =>
          Module.read(json, false).right.map { mod =>
            Manager.modules.add(mod, repo)
          }
          Right("added")
        }.getOrElse(Left("No repos to add module to"))
      case _ => Left("module json required")
    }
}
