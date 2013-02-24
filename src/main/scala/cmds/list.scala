package adept.cmds

import adept.{ Config, Files }
import java.io.{ File, FilenameFilter }

object List extends Cmd {

  val OrgAndName = """(.+):(.+)""".r

  def apply(args: Array[String]) =
    args match {
      case Array(name) =>
        name match {
          case OrgAndName(org, name) =>
            ls(Files.matching(org), Files.matching(name))
          case name =>
            if (name contains(".")) ls(Files.matching(name), Files.any)
            else ls(Files.any, Files.matching(name))
        }
      case _ =>
        ls(Files.any, Files.any)
    }

  private def ls(orgf: FilenameFilter, namef: FilenameFilter) = {
    val repos = Config.reposDir
    if (!repos.exists) Left("No metadata repos exist")
    else {
      repos.listFiles.map { repo =>
        val metadata = new File(repo, "metadata")
        if (!metadata.exists || !metadata.isDirectory) println("No metadata exists in repo %s" format repo.getName)
        else {
          metadata.listFiles(orgf).map { org =>
            if (org.isDirectory) {
              org.listFiles(namef).map { module =>
                println("- %s" format module.getName)
                module.listFiles.map(_.getName).foreach(version => println("  %s" format(version)))
              }
            }
          }
        }
      }
      Right("")
    }
  }
}
