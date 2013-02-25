package adept

object Config {
  import java.io.File
  import scala.util.Properties.envOrNone
  def adeptDir = envOrNone("ADEPT_DIR")
                  .map(new File(_))
                  .getOrElse(new File(System.getProperty("user.home"), ".adept"))
  def metaDir = new File(adeptDir, "meta")
  def reposDir = new File(metaDir, "repos")
}
