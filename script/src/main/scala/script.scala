package adept

import xsbti.{ AppMain, AppConfiguration }

object Script {
  def apply(args: Array[String]) =
    args.take(1)
        .headOption
        .flatMap(cmds.Commands.get)
        .getOrElse(cmds.Help)(args.drop(1)).fold({ err =>
          println(err)
          1
        }, { ok =>
          println(ok)
          0
        })
}

class Script extends AppMain {
  def run(conf: AppConfiguration) =
    new Exit(Script(conf.arguments))
}

class Exit(val code: Int) extends xsbti.Exit

