package adept.manage

trait Repos {
  def add(repo: String, branch: Option[String] = None): Either[String, String]
  def delete(repo: String): Either[String, String]
  def update: Either[String, String]
  def list: Iterable[String]
  def lock(repo: String, sha: Option[String] = None): Either[String, String]
  def unlock(repo: String): Either[String, String]
}

object GitRepos extends Repos {
  import java.io.{ File, PrintWriter }
  import org.eclipse.jgit.api.CreateBranchCommand
  import org.eclipse.jgit.lib.TextProgressMonitor
  import adept.{ Config, Files, Git }

  def add(repo: String, maybeBranch: Option[String] = None) = {
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
      val branch = maybeBranch.getOrElse("master")
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
  }

  def delete(repo: String) = {
    val repos = Config.reposDir
    if (repos.exists) {      
      val repoDir = new File(repos, repo)
      if (repoDir.exists) {
        Files.rm(repoDir)
        Right("Deleted repo %s" format repo)
      } else Left("Repo %s does not exist" format repo)            
    } else Left("no repos to delete")
  }

  def list: Iterable[String] =
    Config.reposDir match {
      case ne if (!ne.exists) => Array.empty[String]
      case repos =>
        repos.listFiles.map(_.getName)
    }

  def update: Either[String, String] = {
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

  def lock(repo: String, sha: Option[String] = None) = {
    import org.eclipse.jgit.revwalk.{ RevCommit, RevWalk }
    import org.eclipse.jgit.api.errors.{ InvalidRefNameException, RefNotFoundException }
    import org.eclipse.jgit.errors.InvalidObjectIdException

    val repos = Config.reposDir
    if (!repos.exists) repos.mkdirs()
    val target = new File(repos, repo)
    if (!target.exists) Left("Repo %s does not exist" format repo)
    else {
      val lockfile = new File(target, ".lock")
      if (lockfile.exists) Left("Repo %s is already locked" format repo)
      else {
        lockfile.createNewFile
        val git = Git.repo(target)
        val log = git.log().call().iterator()
        sha.map { s =>
          def find: Option[RevCommit] =
            if (!log.hasNext) None
            else {
              val rev = log.next
              if (rev.name.startsWith(s)) Some(rev)
              else find
            }
          find.map { commit =>
             try {
               git.checkout()
                  .setForce(true)
                  .setStartPoint(commit)
                  .call
               Files.write(lockfile)(s)
               Right("Repo %s locked at %s" format(repo, sha))
             } catch {
              case nf: RefNotFoundException     => Left("Commit %s not found" format sha)
              case iv: InvalidRefNameException  => Left("Invalid (refname) commit %s" format sha)
              case id: InvalidObjectIdException => Left("Invalid (objectid) commit %s" format sha)
             }
           }.getOrElse(Left("Could not find commit %s" format s))
        }.getOrElse {
          val last = if (log.hasNext) log.next().getId().abbreviate(8).name
                     else "(no commits)"
          Files.write(lockfile)(last)
          Right("Repo %s locked at %s" format(repo, sha))  
        }       
      }
    }
  }

  def unlock(repo: String) = {
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
  }
}
