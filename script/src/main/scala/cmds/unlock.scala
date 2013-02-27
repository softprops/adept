package adept.cmds

import adept.Manager

object Unlock extends Cmd {
  def apply(args: Array[String]) = 
    args match {
      case Array(repo) =>
        Manager.repos.unlock(repo)
      case _ =>
        Left("repo required")
    }
}
