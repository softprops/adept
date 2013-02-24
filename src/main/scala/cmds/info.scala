package adept.cmds

import java.io.File
import adept.{ Config, Files, Module }
import semverfi.Version

object Info extends Cmd {
  def apply(args: Array[String]) = 
    args match {
      case Array(name) =>
        info(name)
      case Array(name, version) =>
        info(name, Some(version))
    }

   def info(name: String, version: Option[String] = None): Either[String, String] = {

     def showModule(f: File) = {
       val json = new File(f, "module.json")
       if (!json.exists) println("module.json missing for %s" format f.getName)
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
     }

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
                   case ary if (ary.isEmpty) => println(version.map(v => "version %s not defined" format v).getOrElse("no versions defined"))
                   case Array(vers) =>
                     showModule(vers)
                   case _ =>
                     // todo: sort and take latest if more than one are returned
                     versions.map(v => (Version(v.getName), v)).sortBy(_._1).reverse.headOption.map {
                       case (_, latest) =>
                         showModule(latest)
                     }
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
