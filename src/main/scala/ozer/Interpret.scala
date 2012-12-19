package ozer

import ozer.parsing._
import ozer.handlers._

trait Interpret {
  def sourceHandler: SourceHandler
  def screenHandler: ScreenHandler
  def dbHandler:     DbHandler
  
  def listSources: Unit = {
    sourceHandler.printList   
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

  def createDb(dir: String) = {
    dbHandler.create(dir)
  }

  def updateDb() = {
    dbHandler.update
  }

  def dbStatus() = {
    dbHandler.status
  }

  def unsupported(command: Command) = {
    screenHandler.println(command.toString + " not supported yet")
  }

  def apply(command: Command): Unit = command match {
    case Error(message)    => handleError(message)
    case Source.List       => listSources
    case Source.Add(files) => addSources(files)
    case Source.Rm(files)  => removeSources(files)
    case Db.Create(dir)    => createDb(dir)
    case Db.Update         => updateDb()
    case Db.Status         => dbStatus()
    case cmd               => unsupported(cmd)
  }
}
