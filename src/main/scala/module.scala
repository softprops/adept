package adept

import semverfi._

case class Artifact(hash: String)

// this is an extention point for behavior beyond value attribution
// i.e. filtering, requirements, ect
case class Configuration(map: Map[String, Attribute]) extends Attributed {
  def attr(name: String) = map.get(name)
}

trait ModuleLike extends Attributed {

  // required

  def organization: String  
  def name: String
  def version: SemVersion
  def artifacts: Iterable[Artifact]

  // optional

  def dependencies: Iterable[Module]
  def apiURL: Option[String]
  def mainClass: Option[String]

  // per configuration attributes

  def config(name: String): Option[Configuration]
}

case class Module(organization: String,
                  name: String,
                  version: SemVersion,                  
                  artifacts: Set[Artifact] = Set.empty[Artifact],
                  dependencies: Set[Module] = Set.empty[Module],
                  apiURL: Option[String] = None,
                  mainClass: Option[String] = None,
                  attributed: Option[Attributed] = None)
   extends ModuleLike {
  def dependency(module: Module) =
    copy(dependencies = dependencies + module)
  def attr(name: String) = attributed.flatMap(_.attr(name))
  def config(name: String) = attributed.flatMap(_.attr(name)).flatMap {
    case c: Configuration => Some(c)
  }
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
        val result: Either[String, Module] =
          Right(Module(org,
                       name,
                       Version(version),
                       attributed = Some(parseAttributes(1, json))))
        val mod = (result /: parseDeps(json).flatten) {
          case (module, (deporg, depname, depversion)) =>
            module.fold(Left(_), { mod =>
              // todo: graph by org/name/version instead of just by name/version
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

  private def parseAttributes(i: Int, obj: JValue): Attributed = {
    // for comp had wierd behavior of injecting inner object keys into outter object scope
    val JObject(fields) = obj
    asConfig(fields.map { case (key, value) => (key, parseAttribute(value)) }
                   .filter { case (_, v) => v.isDefined }
                   .map { case (k, v) => (k, v.get) }
                   .toMap)
  }

  private def parseAttribute(obj: JValue): Option[Attribute] =
    obj match {
      case JString(str)    => Some(StringAttribute(str))
      case JBool(bool)     => Some(BoolAttribute(bool))
      case JDouble(dub)    => Some(DoubleAttribute(dub))
      case JDecimal(dec)   => Some(DecimalAttribute(dec))
      case JNull           => None
      case JNothing        => None
      case JInt(int)       => Some(IntAttribute(int.toInt))
      case JArray(xs)      => Some(Attributes((xs map parseAttribute).flatten))
      case job: JObject    =>
        Some(parseAttributes(2, job))
    }

  private def asConfig(map: Map[String, Attribute]): Configuration =
    Configuration(map)
}
