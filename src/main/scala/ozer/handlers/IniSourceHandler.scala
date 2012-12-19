package ozer.handlers

import UnpureHandlers._
import ozer.Globals

object PureSourceHandler extends Globals {
  import ozer.util.Inis.Ini
  
  def list(config: Ini): List[String] = {
    val section = config.getOrElse(Config.SourceSection, Map.empty)
    section.getOrElse(Config.SourceDir, List.empty)
  }

  def remove(
      sources: List[String],
      files: List[String],
      areSame: (String, String) => Boolean):
    (List[String], List[String]) = {

    var newSources = sources
    var warnings = List.empty[String]

    files.foreach { file => 
      val validFile = newSources.exists(source => areSame(source , file))
      if (validFile) {
        newSources = newSources.filter(source => !areSame(source, file))
      } else {
        warnings ::= "Can't remove source '" + file + "' - source doesn't exist."
      }
    }

    (newSources, warnings)
  }

  def add(
      sources: List[String], 
      files: List[String], 
      areSame: (String, String) => Boolean, 
      valid: (String => Boolean)): 
    (List[String], List[String]) = {

    var newSources = sources
    var warnings = List.empty[String]

    def duplicate_?(file: String): Boolean = {
      if (newSources.contains(file)) return true
      
      newSources.foreach { source => 
        if (areSame(source, file)) return true 
      }

      false
    }

    files.foreach { file => 
      if (valid(file)) {
        if (duplicate_?(file)) {
          warnings ::= "Warning not adding duplicate: '" + file + "'."
        } else {
          newSources ::= file
        }
      } else {
        warnings ::= "Error file: '" + file + "' doesn't exist or is invalid (skipping)."
      }
    }
    
    (newSources, warnings)
  }
}

class IniSourceHandler(
    val fileSystemHandler: FilesystemHandler,
    val screenHandler: ScreenHandler) 
  extends SourceHandler
  with HasIniConfig
  with Globals {

  private val areSame = fileSystemHandler.areSame(_, _)

  def printList(): Unit = {
    PureSourceHandler.list(ini).foreach { line =>
      screenHandler.println(line)
    }
  }
  
  def list(): List[String] = {
     PureSourceHandler.list(ini)
  }
  
  private[this] def writeSources(newSources: List[String]) = {
    write(Config.SourceSection, Config.SourceDir, newSources)
  }

  def add(files: List[String]) {
    val sources = PureSourceHandler.list(ini)
    val valid = fileSystemHandler.exists(_)
    val (newSources, warnings) = PureSourceHandler.add(sources, files, areSame, valid)

    warnings.foreach(w => screenHandler.println(w))
    writeSources(newSources)
  }

  def remove(files: List[String]) {
    val sources = PureSourceHandler.list(ini)
    val (newSources, warnings) = PureSourceHandler.remove(sources, files, areSame)

    warnings.foreach(w => screenHandler.println(w))
    writeSources(newSources)
  }
}
