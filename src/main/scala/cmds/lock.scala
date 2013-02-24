package adept.cmds

import adept.{ Config, Files, Git }

object Lock extends Cmd {
  import java.io.File
  import org.eclipse.jgit.revwalk.{ RevCommit, RevWalk }
  import org.eclipse.jgit.api.errors.{ InvalidRefNameException, RefNotFoundException }
  import org.eclipse.jgit.errors.InvalidObjectIdException

  def apply(args: Array[String]) = 
    args match {
      case Array(repo, sha) =>
        val repos = Config.reposDir
        if (!repos.exists) repos.mkdirs()
          val target = new File(repos, repo)
          if (!target.exists) Left("repo %s does not exist" format repo)
          else {
            val lockfile = new File(target, ".lock")
            if (lockfile.exists) Left("Repo %s is already locked" format repo)
            val git = Git.repo(target)
            try {
              val it = git.log.call.iterator
              def find: Option[RevCommit] =
                if (!it.hasNext) None
                else {
                  val rev = it.next
                  if (rev.name.startsWith(sha)) Some(rev)
                  else find
                }
              find.map { commit =>
                git.checkout()
                 .setForce(true)
                 .setStartPoint(commit)
                 .call              
                Files.write(lockfile)(sha)
                Right("Repo %s locked at %s" format(repo, sha))
              }.getOrElse(Left("Invalid commit %s" format sha))
            } catch {
              case nf: RefNotFoundException     => Left("Commit %s not found" format sha)
              case iv: InvalidRefNameException  => Left("Invalid (refname) commit %s" format sha)
              case id: InvalidObjectIdException => Left("Invalid (objectid) commit %s" format sha)
            }
          }
        case Array(repo) =>
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
              val sha = if (log.hasNext) log.next().getId().abbreviate(8).name else "(no commits)"
              Files.write(lockfile)(sha)
              Right("Repo %s locked at %s" format(repo, sha))
            }
          }
        case _ => Left("repo required")
      }
  }
