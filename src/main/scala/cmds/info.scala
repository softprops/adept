package adept.cmds

import java.io.File
import adept.{ Config, Files, Module }

object Info extends Cmd {
  def apply(args: Array[String]) = 
    args match {
      case Array(name) =>
        info(name)
      case Array(name, version) =>
        info(name, Some(version))
    }

   def info(name: String, version: Option[String] = None): Either[String, String] = {
     val repos = Config.reposDir
     if (!repos.exists) Left("No metadata repos exist")
     else {
       repos.listFiles.map { repo =>
         val metadata = new File(repo, "metadata")
         if (!metadata.exists || !metadata.isDirectory) println("No metadata exists in repo %s" format repo.getName)
         else {
           metadata.listFiles(Files.any).map { org =>
             if (org.isDirectory) {
               org.listFiles(Files.matching(name)).map { module =>
                 println("- %s" format module.getName)
                 val versions = version.map(v => module.listFiles(Files.matching(v))).getOrElse(
                   module.listFiles(Files.any)
                 )
                 versions match {
                   case ary if (ary.isEmpty) => println("No versions defined.")
                   case Array(vers) =>
                     val json = new File(vers, "module.json")
                     if (!module.exists) println("module.json missing for %s" format vers.getName)
                     else {
                       Module.read(json).fold(println, { mod =>
                         println("version: %s" format mod.version)
                         println("dependencies")
                         if (mod.dependencies.isEmpty) println("(none)")
                         else mod.dependencies.foreach { d =>
                           println("%s / %s @ %s" format(d.organization, d.name, d.version))
                         }
                       })
                     }
                   case _ =>
                     // todo: sort and take latest if more than one are returned
                     println("select a version: %s" format versions.map(_.getName).mkString(", "))
                 }
                }
              }
            }            
          }
        }
        Right("")
      }
   }
}
