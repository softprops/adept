package adept.cmds

import adept.Config

object Unlock extends Cmd {
  import java.io.File

  def apply(args: Array[String]) = 
    args match {
      case Array(repo) =>
        val repos = Config.reposDir
        if (!repos.exists) repos.mkdirs()
        val target = new File(repos, repo)
        if (!target.exists) Left("repo %s does not exist" format repo)
        else {
          val lockfile = new File(target, ".lock")
          if (!lockfile.exists) Left("Repo %s is not locked" format repo)
          else {
            lockfile.delete
            Right("Repo %s unlocked" format repo)
          }
        }
      case _ => Left("repo required")
    }
}
