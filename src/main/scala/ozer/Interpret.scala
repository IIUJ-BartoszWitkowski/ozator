package ozer

import ozer.parsing._

trait Interpret {
  def sourceHandler: SourceHandler
  def screenHandler: ScreenHandler
  
  def listSources: Unit = {
    sourceHandler.list.foreach { source =>
      screenHandler.println(source)
    }
  }

  def handleError(message: String): Unit = {
    screenHandler.println(message)
  }

  def addSources(files: List[String]) = {
    sourceHandler.add(files)
  }

  def removeSources(files: List[String]) = {
    sourceHandler.remove(files)
  }

  def unsupported(command: Command) = {
    screenHandler.println(command.toString + " not supported yet")
  }

  def apply(command: Command): Unit = command match {
    case Error(message)    => handleError(message)
    case Source.List       => listSources
    case Source.Add(files) => addSources(files)
    case Source.Rm(files)  => removeSources(files)
    case cmd               => unsupported(cmd)
  }
}
