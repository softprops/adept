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
  def dependency(org: String, name: String, version: SemVersion) =
    copy(dependencies = dependencies + Module(org, name, version))
}

object Module {
  import org.json4s._
  import org.json4s.native.JsonMethods._

  def read(in: String): Either[String, Module] = {
    val json = parse(in)
    parseBasics(json).headOption.map {
      case (org, name, version) =>
        val mod = (Module(org, name, Version(version)) /: parseDeps(json).flatten) {
          case (builder, (deporg, depname, depversion)) =>
            builder.dependency(deporg, depname, Version(depversion))
        }
        parseArtifacts(json) match {
          case Nil =>
            println("warning: module %s@%s does not declare any artifacts" format(name, version))
            Right(mod)
          case arts =>
            Right(mod.copy(artifacts = arts.map(Artifact(_)).toSet))
        }
    }.getOrElse(Left("Missing required org, name, or version"))
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
