package ozer.handlers

import ozer.Globals
import java.io.File

trait SourceHandler {
  val section = "general"
  val key = "sources"

  def list(): Unit
  def add(files: List[String]): Unit 
  def remove(files: List[String]): Unit
}

trait ScreenHandler {
  def println(message: String)
}

trait FilesystemHandler {
  def homeDir: File
  def fileFromHome(name: String): File
  def exists(path: String): Boolean
  def isSymlink(path: String): Boolean
  def areSame(left: String, right: String): Boolean
  def expand(name: String): String
}

object UnpureHandlers {
  trait LinuxSymlinks {
    import sys.process._ 
    type SanitizedPath = Option[String]
    type CorrectPath = Some[String]

    private[this] def testSeq(params: Seq[String]): Boolean = {
      val returnValue = ("test" +: params).!
      returnValue == 0
    }

    private[this] def test(params: String*): Boolean = {
      testSeq(params)
    }

    private[this] def isSymlink(path: SanitizedPath): Boolean = {
      if (path.isDefined) {
        test("-h", path.get)
      } else {
        false
      }
    }

    def isSymlink(path: String): Boolean = {
      isSymlink(sanitize(path))  
    }

    private[this] def areSame(left: SanitizedPath, right: SanitizedPath) = {
      if (left.isDefined && right.isDefined) {
        test(left.get, "-ef", right.get)
      } else {
        false
      }
    }

    def areSame(left: String, right: String): Boolean = {
      areSame(sanitize(left), sanitize(right))
    }

    def sanitize(path: String): SanitizedPath = {
      val file = new File(path)
      if (file.exists) Some(file.getAbsolutePath)
      else             None
    }
  }

  object UnpureFilesystemHandler 
    extends FilesystemHandler 
    with LinuxSymlinks 
    with Globals {
  
    override def homeDir = {
      val dir = new File(OzerHome)
      if (!dir.exists) dir.mkdirs
      dir 
    }
    
    override def exists(path: String) = new File(path).exists
    
    override def expand(file: String) = new File(file).getAbsolutePath
    
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

  object UnpureScreenHandler extends ScreenHandler {
    def println(message: String) = {
      System.out.println(message)
    }
  }
}
