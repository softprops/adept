package adept

package object cmds {
  trait Cmd extends (Array[String] => Either[String, String])

  val Commands: Map[String, Cmd] = Map(
    "help"     -> Help,
    "repos"    -> Repos,
    "update"   -> Update,
    "diff"     -> Diff,
    "info"     -> Info,
    "list"     -> List,
    "search"   -> Search,
    "lock"     -> Lock,
    "unlock"   -> Unlock)
}
