package adept.manage

import java.io.{ File, FilenameFilter }
import adept.{ Config, Files, Module }
import semverfi.Version

trait Modules {
  type Listing = Iterable[(String, Seq[String])]
  def byOrganization(org: String): Listing
  def byName(name: String): Listing
  def byOrgAndName(org: String, name: String): Listing
  def list: Listing
  def info(name: String, version: Option[String] = None): Either[String, Module]
}

object FsModules extends Modules {
  def byOrganization(org: String) =
    ls(Files.matching(org), Files.any)

  def byName(name: String) = 
    ls(Files.any, Files.matching(name))
 
  def byOrgAndName(org: String, name: String) =
    ls(Files.matching(org), Files.matching(name))

  def list =
    ls(Files.any, Files.any)

  def info(name: String, version: Option[String] = None): Either[String, Module] = {
    def extractModule(f: File): Either[String, Module] =
      new File(f, "module.json") match {
        case ne if (!ne.exists) => Left("module.json missing for %s" format f.getName)
        case json => Module.read(json)
      }
    val repos = Config.reposDir
    if (!repos.exists) Left("No metadata repos exist")
    else ((Left("Could not find module"): Either[String, Module]) /: repos.listFiles) {
      case (result, repo) =>
        val metadata = new File(repo, "metadata")
        if (!metadata.exists || !metadata.isDirectory) result
        else (result /: metadata.listFiles(Files.any).filter(_.isDirectory)) {
          case (result, org) =>
            (result /: org.listFiles(Files.matching(name))) {
              case (result, module) =>
                val versions = version.map(v => module.listFiles(Files.matching(v)))
                                      .getOrElse(module.listFiles(Files.any))
                versions match {
                  case ary if (ary.isEmpty) =>
                    Left(version.map(v => "version %s not defined" format v)
                                .getOrElse("no versions defined"))
                  case Array(vers) =>
                    extractModule(vers)
                  case _ =>
                    (versions.map(v => (Version(v.getName), v))
                             .sortBy(_._1)
                             .reverse
                             .headOption.map {
                               case (_, latest) =>
                                 extractModule(latest)
                             }).get
                }
            }
          }
        }
   }

  private def ls(orgf: FilenameFilter, namef: FilenameFilter): Listing = {
    val repos = Config.reposDir
    if (!repos.exists) List.empty[(String, Seq[String])]
    else {
      (List.empty[(String, Seq[String])] /: repos.listFiles.toList) {
        case (result, repo) =>
          val metadata = new File(repo, "metadata")
          if (!metadata.exists || !metadata.isDirectory) result
          else {
            val modules = metadata.listFiles(orgf).toList.filter(_.isDirectory).map { org =>
              org.listFiles(namef).toList.map { module =>                
                val versions: Seq[String] = module.listFiles
                                                  .map(v => (Version(v.getName), v.getName))
                                                  .sortBy(_._1)
                                                  .reverse
                                                  .map(_._2)
                (module.getName, versions)
              }
            }
            result ::: modules.flatten
          }
      }
    }
  }
}
