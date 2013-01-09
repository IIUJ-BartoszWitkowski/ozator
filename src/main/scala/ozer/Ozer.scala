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

    lazy val fileSystemHandler = UnpureFilesystemHandler
    override lazy val screenHandler = UnpureScreenHandler
    override lazy val sourceHandler = new IniSourceHandler(fileSystemHandler, screenHandler)
    override lazy val dbHandler = new DbHandlerImpl(sourceHandler, screenHandler, fileSystemHandler)
    override lazy val movieHandler = new MovieHandlerImpl(screenHandler, dbHandler, fileSystemHandler)
    override lazy val tagHandler = new TagHandlerImpl(screenHandler, fileSystemHandler, movieHandler, dbHandler)
    override lazy val lsHandler = 
      new LsHandlerImpl(screenHandler, fileSystemHandler, dbHandler, movieHandler, tagHandler)
    override lazy val autotagHandler 
      = new AutotagHandlerImpl(screenHandler, fileSystemHandler, lsHandler, movieHandler, tagHandler)
    
  }

  def main(args: Array[String]) {
    debug("Started with " + args.mkString("(", ",", ")"))
    val command = Parse(args)
    interpret(command)
  }
}
