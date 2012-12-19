package ozer

import ozer.parsing.Parse

trait Log {
  val DEBUG = false
  def debug(str: String) = if (DEBUG) println(str)
}

object Ozer extends Log {
  val interpret: Interpret = new Interpret {
    import ozer.handlers._ 
    import ozer.handlers.UnpureHandlers._ 

    override lazy val sourceHandler = new IniSourceHandler(UnpureFilesystemHandler, screenHandler)
    override lazy val screenHandler = UnpureScreenHandler
    override lazy val dbHandler = new DbHandlerImpl(sourceHandler, screenHandler, UnpureFilesystemHandler)
  }

  def main(args: Array[String]) {
    debug("Started with " + args.mkString("(", ",", ")"))
    val command = Parse(args)
    interpret(command)
  }
}
