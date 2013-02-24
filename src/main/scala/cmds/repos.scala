package adept.cmds

import adept.{ Config, Git }

object Repos extends Cmd {
  import java.io.{ File, PrintWriter }
  import org.eclipse.jgit.api.CreateBranchCommand
  import org.eclipse.jgit.lib.TextProgressMonitor
  import scala.collection.JavaConverters._

  def apply(args: Array[String]) =
    args.toList match {
      case "add" :: repo :: rest  =>
        val repos = Config.reposDir
        if (!repos.exists) repos.mkdirs()
        val target = new File(
          repos,
          repo.split('/')
              .reverse
              .headOption
              .flatMap(_.split("[.]").headOption)
              .getOrElse(
                "repo-%s" format(System.currentTimeMillis)
              )
        )
        if (target.exists && new File(target, ".git").exists) Left(
          "repo %s already added" format repo)
        else {
          val branch = rest.headOption.getOrElse("master")
          println("cloning %s#%s to %s" format(repo, branch, target))

          // clone
          val g = Git.clone(repo, target, branch)
                     .setProgressMonitor(
                       new TextProgressMonitor(new PrintWriter(System.out))
                     )
                     .call
            
          // set a branch for upstream changes
          g.branchCreate() 
           .setName(branch)
           .setUpstreamMode(CreateBranchCommand.SetupUpstreamMode.SET_UPSTREAM)
           .setStartPoint("origin/%s" format branch)
           .setForce(true)
           .call
          Right("Cloned repo to %s" format target)
        }

      case "delete" :: repo :: Nil =>
        val repos = Config.reposDir
        if (repos.exists) {
          def delete(f: File): Unit =
            if (!f.isDirectory) f.delete()
            else {
              f.listFiles.foreach(delete)
              f.delete()
            }
          val repoDir = new File(repos, repo)
          if (repoDir.exists) {
            delete(repoDir)
            Right("Deleted repo %s" format repo)
          } else Left("Repo %s does not exist" format repo)            
        } else Left("no repos to delete")
      case Nil =>
        val repos = Config.reposDir
        if (!repos.exists) Right("")
        else Right(repos.listFiles.map(_.getName).mkString("\n"))
      case args =>
        Left("unhandled arguments %s" format args.toList)
    }
}
