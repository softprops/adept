package adept.cmds

import adept.Manager

object Lock extends Cmd {
  def apply(args: Array[String]) = 
    args match {
      case Array(repo, sha) =>
        Manager.repos.lock(repo, Some(sha))
      case Array(repo) =>
        Manager.repos.lock(repo)
      case _ => Left("repo required")
    }
  }
