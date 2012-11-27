package ozer

import java.io.File

trait SourceHandler {
  val section = "general"
  val key = "sources"

  def list(): List[String]
  def add(files: List[String]): Unit 
  def remove(files: List[String]): Unit
}

trait ScreenHandler {
  def println(message: String)
}

trait FilesystemHandler {
  def homeDir: File
  def fileFromHome(name: String): File
}


object UnpureHandlers {
  object UnpureFilesystemHandler 
    extends FilesystemHandler 
    with Globals {
  
    override def homeDir = {
      val dir = new File(OzerHome)
      if (!dir.exists) dir.mkdirs
      dir 
    }
    override def fileFromHome(name: String) = new File(homeDir, name)
  }

  trait HasIniConfig extends Globals {
    def fileSystemHandler: FilesystemHandler
    
    import ozer.util.Inis
    import Inis._
    
    def ini = {
      import ozer.util.Inis
      val iniFile = fileSystemHandler.fileFromHome(Config.FileName)
      Inis.fromFile(iniFile)
    }

    def write(ini: Ini) = {
      val file = fileSystemHandler.fileFromHome(Config.FileName)
      if (!file.exists) file.createNewFile
      Inis.toFile(ini, file)
    }
  }

  class IniSourceHandler(val fileSystemHandler: FilesystemHandler) 
    extends SourceHandler
    with HasIniConfig
    with Globals {

    def list(): List[String] = {
      val section = ini.getOrElse(Config.SourceSection, Map.empty)
      section.getOrElse(Config.SourceDir, List.empty)
    }
    
    private[this] def modify(f: List[String] => List[String]) {
      val section: Map[String, List[String]] = ini.getOrElse(Config.SourceSection, Map.empty)
      val key = Config.SourceDir
      
      val newList = f(section.getOrElse(key, List.empty))
      
      val newIni = ini.updated(Config.SourceSection, section.updated(key, newList))
      write(newIni)
    }

    def add(files: List[String]) {
      modify(sources => (sources ++ files).distinct)
    }

    def remove(files: List[String]) {
      modify { sources =>
         sources.filter { source => !files.contains(source) }.distinct
      }
    }
  }

  object UnpureScreenHandler extends ScreenHandler {
    def println(message: String) = {
      System.out.println(message)
    }
  }
}
