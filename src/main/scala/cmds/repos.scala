package adept.cmds

import adept.Manager

object Repos extends Cmd {
  def apply(args: Array[String]) =
    args.toList match {
      case "add" :: repo :: rest  =>
        Manager.repos.add(repo, rest.headOption)
      case "delete" :: repo :: Nil =>
        Manager.repos.delete(repo)
      case Nil =>
        Right(Manager.repos.list.mkString("\n"))      
      case args =>
        Left("unhandled arguments %s" format args.toList)
    }
}
