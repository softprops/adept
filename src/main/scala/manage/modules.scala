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
  def info(name: String, version: Option[String] = None, recurse: Boolean = false): Either[String, Module]
  def graph(name: String, version: Option[String] = None): Either[String, Module]
  def add(mod: Module, repo: String): Either[String, String]
}

object FsModules extends Modules {
  val ModuleFile = "module.json"

  def byOrganization(org: String) =
    ls(Files.matching(org), Files.any)

  def byName(name: String) = 
    ls(Files.any, Files.matching(name))
 
  def byOrgAndName(org: String, name: String) =
    ls(Files.matching(org), Files.matching(name))

  def list =
    ls(Files.any, Files.any)

  def add(mod: Module, repo: String): Either[String, String] = {
    val repos = Config.reposDir
    if (!repos.exists) Left("No metadata repos exist")
    else {
      repos.listFiles(Files.matching(repo)) match {
        case empt if (empt.isEmpty) =>
          Left("Repo %s does not exist" format repo)
        case Array(rep) =>
          // make /com.mycompany/bippy/version/module.json
          val metadataDir = new File(rep, "metadata")
          if (!metadataDir.exists) metadataDir.mkdirs()
          val orgDir = new File(metadataDir, mod.organization)
          if (!orgDir.exists) orgDir.mkdirs()
          val moduleDir = new File(orgDir, mod.name)
          if (!moduleDir.exists) moduleDir.mkdirs()
          val versionDir = new File(moduleDir, mod.version.toString)
          val versionFile = new File(versionDir, ModuleFile)
          if (versionDir.exists && versionFile.exists) Left(
            "%s@%s already exists" format(mod.name, mod.version))
          else {
            if (!versionDir.exists) versionDir.mkdirs()
            versionFile.createNewFile()
            Module.write(mod)(versionFile)
            Right("ok")
          }
      }
    }
  }

  def graph(name: String, version: Option[String] = None): Either[String, Module] =
    info(name, version, recurse = true)

  def info(name: String, version: Option[String] = None, recurse: Boolean = false): Either[String, Module] = {
    def extractModule(f: File): Either[String, Module] =
      new File(f, ModuleFile) match {
        case ne if (!ne.exists) =>
          Left("%s missing for %s" format(ModuleFile, f.getName))
        case json =>
          Module.read(json, recurse)
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
