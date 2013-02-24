package adept.cmds

import adept.Script

object Help extends Cmd {
  def apply(args: Array[String]) = Right("""
     | Usage:
     |
     |     adept <command> <args>
     |
     | Commands:
     |
     |     %s""".stripMargin.format(Commands.keys.mkString(", ")))
}
