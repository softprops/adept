package adept

import adept.manage._

object Manager {
  def repos: Repos = GitRepos
  def modules: Modules = FsModules
}
