package ozer

import ozer.parsing.Parse

trait Log {
  val DEBUG = true
  def debug(str: String) = if (DEBUG) println(str)
}

object Ozer extends Log {
  val interpret: Interpret = new Interpret {
    import ozer.UnpureHandlers._ 

      override val sourceHandler = new IniSourceHandler(UnpureFilesystemHandler)
      override val screenHandler = UnpureScreenHandler
  }

  def main(args: Array[String]) {
    debug("Started with " + args.mkString("(", ",", ")"))
    val command = Parse(args)
    interpret(command)
  }
}
