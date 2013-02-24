package adept

import java.io.File
import org.eclipse.jgit.api.{ Git => JGit }
import org.eclipse.jgit.lib.RepositoryBuilder

object Git {
  def repo(target: File) =
    JGit.wrap(new RepositoryBuilder()
             .setGitDir(new File(target, ".git"))
             .readEnvironment()
             .findGitDir()
             .build())

  def clone(repo: String, target: File, branch: String) =
    JGit.cloneRepository()
       .setCloneAllBranches(true)
       .setURI(repo)
       .setDirectory(target)
       .setBranch(branch)
}
