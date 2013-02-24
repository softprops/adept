package adept

object Config {
  import java.io.File
  def baseDir = new File(System.getProperty("user.home"), ".adept")
  def metaDir = new File(baseDir, "meta")
  def reposDir = new File(metaDir, "repos")
}
