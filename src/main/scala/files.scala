package adept

object Files {
  import java.io.{ File, FilenameFilter, FileWriter }
  import scala.util.matching._

  def write(f: File)(content: String) = {
    val w = new FileWriter(f)
    w.write(content)
    w.flush
    w.close
  }
  
  def rm(f: File): Boolean = {    
    if (!f.isDirectory) f.delete()
    else {
      f.listFiles.foreach(rm)
      f.delete()
    }
  }

  case class Like(pat: String) extends FilenameFilter {
    def accept(dir: File, name: String) =
      new Regex("^."+pat+".*$")
           .findFirstMatchIn(name)
           .isDefined
  }

  case class Matching(target: String) extends FilenameFilter {
    def accept(dir: File, name: String) = name == target
  }

  object Any extends FilenameFilter {
    def accept(dir: File, name: String) = true
  }

  def matching(target: String): FilenameFilter = Matching(target)
  def any: FilenameFilter = Any
  def like(pat: String): FilenameFilter = Like(pat)
}
