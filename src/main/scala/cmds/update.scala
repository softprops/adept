package adept.cmds

import adept.Manager

object Update extends Cmd {
  def apply(args: Array[String]) =
    Manager.repos.update
}
