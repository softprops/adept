package adept

import semverfi._

case class Artifact(hash: String)

trait ModuleLike {

  // required

  def organization: String  
  def name: String
  def version: SemVersion
  def artifacts: Iterable[Artifact]

  // optional

  def dependencies: Iterable[Module]
  def apiURL: Option[String]
  def mainClass: Option[String]
}

case class Module(organization: String,
                  name: String,
                  version: SemVersion,                  
                  artifacts: Set[Artifact] = Set.empty[Artifact],
                  dependencies: Set[Module] = Set.empty[Module],
                  apiURL: Option[String] = None,
                  mainClass: Option[String] = None)
   extends ModuleLike {
  def dependency(module: Module) =
    copy(dependencies = dependencies + module)
}

object Module {
  import org.json4s._
  import org.json4s.native.JsonMethods._
  import org.json4s.JsonDSL._
  import java.io.File
  
  def write(mod: Module)(to: File) = {
    def simple(m: Module) =
      ("name" -> mod.name) ~
      ("organization" -> mod.organization) ~
      ("version" -> mod.version.toString)
    val json = simple(mod) ~
      ("dependencies" -> mod.dependencies.map(simple)) ~
      ("artifacts" -> mod.artifacts.map(_.hash))
    Files.write(to)(pretty(render(json)))
  }

  def read(f: File, recurse: Boolean = false): Either[String, Module] =
    read(io.Source.fromFile(f).getLines().mkString("\n"), recurse)

  def read(in: String, recurse: Boolean): Either[String, Module] = {
    val json = parse(in)
    parseBasics(json).headOption.map {
      case (org, name, version) =>
        val result: Either[String, Module] = Right(Module(org, name, Version(version)))
        val mod = (result /: parseDeps(json).flatten) {
          case (module, (deporg, depname, depversion)) =>
            module.fold(Left(_), { mod =>
              // todo: graph by org/name/version
              if (recurse) Manager.modules.graph(depname, Some(depversion)).right.map({ dep =>
                mod.dependency(dep)
              }) 
              else Right(mod.dependency(Module(deporg, depname, Version(depversion))))
            })
        }
        parseArtifacts(json) match {
          case Nil =>
            println("warning: module %s@%s does not declare any artifacts" format(name, version))
            mod
          case arts =>
            mod.right.map(_.copy(artifacts = arts.map(Artifact(_)).toSet))
        }
    }.getOrElse(Left("Missing required organization, name, or version"))
  }

  private def parseDeps(obj: JValue) =
    for {
      JObject(fields)                <- obj
      ("dependencies", JArray(deps)) <- fields
      dep                            <- deps
    } yield parseBasics(dep).headOption

  private def parseArtifacts(obj: JValue) =
    for {
      JObject(fields)             <- obj
      ("artifacts", JArray(arts)) <- fields
      JString(art)                <- arts
    } yield art

  private def parseBasics(obj: JValue) =
    for {
      JObject(fields)                 <- obj
      ("organization", JString(org))  <- fields
      ("name", JString(name))         <- fields      
      ("version", JString(version))   <- fields
    } yield (org, name, version)
}
