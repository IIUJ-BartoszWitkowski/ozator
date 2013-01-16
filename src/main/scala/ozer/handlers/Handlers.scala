package ozer.handlers

import ozer.Globals
import ozer.data._
import java.io.File

trait SourceHandler {
  def printList(): Unit
  def list(): List[String]
  def add(files: List[String]): Unit 
  def remove(files: List[String]): Unit
}

trait ScreenHandler {
  def println(message: String)
  def ynChoice(message: String): Boolean
  def nChoice(message: String, n: Int): Int
  def getString(message: String): String
  def getNumeric(message: String, low: Int, hi: Int): String
}

trait FilesystemHandler {
  def homeDir: File
  def fileFromHome(name: String): File
  def exists(path: String): Boolean
  def isSymlink(path: String): Boolean
  def areSame(left: String, right: String): Boolean
  def expand(name: String): String
  def mkDir(dirName: String): Unit
  def ls(dirName: String): Seq[String]
  def basename(fileName: String): String
  def dirname(fileName: String): String
  def link(target: String, linkName: String): Unit
  def separator: String
  def readLink(name: String): String
  def rm(fileName: String)
}

trait DbHandler {
  def ozerDb(): Option[String]
  def allDir(): Option[String]
  def create(dir: String): Unit
  def update(): Unit
  def status(): Unit
  def wasDbCreated(): Boolean
  def dbPath(name: String): String
  def realPath(name: String): Option[String]
  def existsInDb(path: String): Boolean
}

trait MovieHandler {
  def createMovies(fileNames: Seq[String]): Seq[Movie] 
  def exists(name: String): Boolean
  def moviePath(name: String, create: Boolean = false): String
  def listNames(): Seq[String] 
  def listFullPath(): Seq[String] 
}

trait AutotagHandler {
  def autotag(): Unit
}

trait TagHandler {
  def addTagToFile(cathegoryName: String, tagName: String, file: String)
  def existsCathegory(cathegoryName: String): Boolean
  def existsTag(cathegoryName: String, tagName: String): Boolean
  def cathegories(): Seq[String]
  def dirFromCathegory(cathegoryName: String): String
  def dirFromCathegoryAndTag(cathegoryName: String, tag: String): String
  def tagsOfMovie(title: String): Seq[(String, String)]
  def tagsOfFile(file: String): Seq[(String, String)]
  def tagsInCathegory(cathegoryName: String): Seq[String]
  def removeTagFromFile(cathegoryName: String, tagName: String, file: String): Unit
  def removeAllTagsInCathegoryFromFile(cathegoryName: String, file: String): Unit
  def addTagToMovie(cathegoryName: String, tagName: String, title: String, force: Boolean = false) 
}

trait LsHandler {
  def list(): Seq[String]
  def listFullPath(): Seq[String] 
  def listCathegories(): Seq[String]
  def printCathegories(): Unit
  def listNotUpdated(): Seq[String] 
  def printList(): Unit
  def tagsOfFile(file: String): Seq[(String, String)]
  def printTagsOfFile(file: String)
}

trait GrepHandler {
  def print(cathegoryName: String, tagPattern: String, moviePattern: String)
  def grep(cathegoryName: String, tagPattern: String, moviePattern: String): Seq[(String, Seq[String])]
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

  def write(sectionName: String, key: String, newValue: List[String]): Unit = {
    val config = ini
    val section: Map[String, List[String]] = config.getOrElse(sectionName, Map.empty)
    val newConfig = config.updated(sectionName, section.updated(key, newValue))
    write(newConfig)
  }
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
    
    def readLink(name: String): String = {
      val result = Seq("readlink", name).!!
      result.trim
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

    def link(target: SanitizedPath, linkName: String): Boolean = {
      if (target.isDefined) {
        val command = Seq("ln", "-s", target.get, linkName)
        val returnValue = command.!
        (returnValue == 0)
      } else {
        false
      }
    }

    def link(target: String, linkName: String): Unit = {
      if (!link(sanitize(target), linkName)) { 
        throw new RuntimeException(
          "Can't link target: '" + target + "' using link name '" + 
          linkName + "'.")
      }
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

    override def mkDir(dirName: String) = {
      if (!new File(dirName).mkdir) throw new RuntimeException(
        "Couldn't create directory: '" + dirName + "'.")
    }

    override def ls(dirName: String) = {
      val list = new File(dirName).list

      if (list == null) List.empty[String]
      else              list
    }

    override def basename(fileName: String) = new File(fileName).getName

    override def dirname(fileName: String): String = 
      new File(fileName).getParentFile.getAbsolutePath

    override def separator : String = File.separator

    override def rm(fileName: String) = new File(fileName).delete
  }

  object UnpureScreenHandler extends ScreenHandler {
    def println(message: String) = {
      System.out.println(message)
    }
    def ynChoice(message: String): Boolean = {
      System.out.println(message)
      val line = readLine()
      line.toLowerCase == "y"
    }
    def nChoice(message: String, n: Int): Int = {
      while (true) {
        System.out.println(message)
        val line = readLine()
        System.out.println
        if (!line.isEmpty && line.trim.forall(_.isDigit)) {
          val i = line.toInt
          if (i >= 1 && i <= n) return i
        }
      }
      0
    }
    def getNumeric(message: String, low: Int, high: Int): String = {
      while (true) {
        System.out.println(message)
        val line = readLine()
        System.out.println
        if (!line.isEmpty && line.trim.forall(_.isDigit)) {
          val i = line.toInt
          if (i >= low && i <= high) return i.toString
        }
      }
      low.toString
    }

    def getString(message: String): String = {
      System.out.println(message)
      val line = readLine()
      line
    }
  }
}
