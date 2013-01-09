package ozer

import ozer.parsing._
import ozer.handlers._

trait Interpret {
  def sourceHandler: SourceHandler
  def screenHandler: ScreenHandler
  def dbHandler:     DbHandler
  def lsHandler:     LsHandler
  def tagHandler:    TagHandler
  def movieHandler:  MovieHandler = null
  def autotagHandler: AutotagHandler = null
  
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

  def listAll() {
    lsHandler.printList()
  }

  def listTagsOfFile(file: String) {
    lsHandler.printTagsOfFile(file)
  }

  def addTagToFile(cathegoryName: String, tagName: String, file: String) {
    tagHandler.addTagToFile(cathegoryName, tagName, file)
  }

  def removeTagFromFile(cathegoryName: String, tagName: String, file: String) {
    tagHandler.removeTagFromFile(cathegoryName, tagName, file)
  }

  def removeAllTagsInCathegoryFromFile(cathegoryName: String, file: String) {
    tagHandler.removeAllTagsInCathegoryFromFile(cathegoryName, file)
  }

  def autotag() {
    autotagHandler.autotag()
  }

  def unsupported(command: Command) = {
    screenHandler.println(command.toString + " not supported yet")
  }

  def apply(command: Command): Unit = command match {
    case Error(message)                      => handleError(message)
    case Source.List                         => listSources
    case Source.Add(files)                   => addSources(files)
    case Source.Rm(files)                    => removeSources(files)
    case Db.Create(dir)                      => createDb(dir)
    case Db.Update                           => updateDb()
    case Db.Status                           => dbStatus()
    case Ls.Everything                       => listAll()
    case Ls.TagsOfFile(file)                 => listTagsOfFile(file)
    case Tag.AddTagToFile(key, value, name)  => addTagToFile(key, value, name)
    case Tag.RmTagFromFile(key, value, name) => removeTagFromFile(key, value, name)
    case Tag.RmAllTagsFromFile(key, name)    => removeAllTagsInCathegoryFromFile(key, name)
    case Tag.Auto                            => autotag()
    case cmd                                 => unsupported(cmd)
  }
}
