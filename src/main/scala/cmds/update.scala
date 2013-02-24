package adept.cmds

import adept.{ Config, Git }

object Update extends Cmd {
  import org.eclipse.jgit.lib.{ RepositoryBuilder, TextProgressMonitor }
  import org.eclipse.jgit.storage.file.FileRepositoryBuilder
  import org.eclipse.jgit.transport.RefSpec
  import java.io.{ File, PrintWriter }

  def apply(args: Array[String]) = {
    val repos = Config.reposDir
    if (!repos.exists || repos.listFiles.isEmpty) Left(
      "Nothing to update: no metadata repositories configured")
    else {
      repos.listFiles.foreach { f =>          
        val repo = Git.repo(f)
        val lock = new File(f, ".lock")
        if (lock.exists) println(
          "Repo %s locked at %s" format(f.getName, io.Source.fromFile(lock).getLines().mkString("")))
        else {
          println("updating %s..." format f)
          repo.pull()
              .setProgressMonitor(
                new TextProgressMonitor(new PrintWriter(System.out))
              )
              .call        
        }
      }
      Right("repos updated")
    }
  }
}
