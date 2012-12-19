package ozer.handlers

import ozer.Globals
import ozer.util.Inis.Ini

object PureDbHandler extends Globals {
  def dir(config: Ini): Option[String] = {
    val section = config.getOrElse(Config.DbSection, Map.empty)
    section.get(Config.DbDir).map { list => list.head }
  }

  def canCreate(dir: Option[String], existsDir: Boolean): (Boolean, Option[String]) = {
    val wasCreated = dir.isDefined

    if (wasCreated) {
      val warning = "Error - database already created at '" + 
                   dir.get + "'."
      (false, Some(warning))
    } else if (existsDir) {
      val warning = "Error - can't create data base. Target directory non empty"
      (false, Some(warning))
    } else {
      (true, None)
    }
  }
}

class DbHandlerImpl(
    sourceHandler: SourceHandler,
    screenHandler: ScreenHandler,
    val fileSystemHandler: FilesystemHandler)
  extends DbHandler 
  with HasIniConfig {

  private[this] def writeDir(dir: String) = {
    write(Config.DbSection, Config.DbDir, List(dir))
  }

  def dir(): Option[String] = PureDbHandler.dir(ini)

  def create(dirName: String): Unit = {
    val exists = fileSystemHandler.exists(dirName)
    val (canCreate, warning) = PureDbHandler.canCreate(dir, exists)

    if (canCreate) {
      fileSystemHandler.mkDir(dirName)
      val sources = sourceHandler.list()

      for {
        source <- sources
        item <- fileSystemHandler.ls(source)
      } {
        val linkPath = dirName + fileSystemHandler.separator + fileSystemHandler.basename(item)
        val itemPath = source + fileSystemHandler.separator + item
        fileSystemHandler.link(itemPath, linkPath)
      }

      writeDir(dirName)
    } else {
      screenHandler.println(warning.get)
    }
  }
  
  private[this] def link(source: String, item: String, dirName: String): Unit = {
    val linkPath = dirName + fileSystemHandler.separator + fileSystemHandler.basename(item)
    val itemPath = source + fileSystemHandler.separator + item
    fileSystemHandler.link(itemPath, linkPath)
  }

  def listNotUpdated: List[(String, String)] = {
    def contains(dir: String, item: String): Boolean = {
      val links = fileSystemHandler.ls(dir)
      links.foreach { link =>
        val linkPath = dir + fileSystemHandler.separator + link
        if (fileSystemHandler.areSame(item, linkPath)) return true
      }

      false
    }

    if (dir.isDefined) {
      val sources = sourceHandler.list()

      for {
        source <- sources
        item <- fileSystemHandler.ls(source)
        if (!contains(dir.get, source + fileSystemHandler.separator + item))
      } yield (source, item)
    } else {
      List.empty[(String, String)]
    }
  }

  def update(): Unit = {
    val notUpdated = listNotUpdated

    for {
      (source, item) <- listNotUpdated
    } link(source, item, dir.get)
  }
  
  def status(): Unit = {
    val groupedBySources = listNotUpdated.groupBy(_._1)
    for {
      (source, notUpdated) <- groupedBySources
    } {
      val (_, items) = notUpdated.unzip
      screenHandler.println(source + ":")
      items.foreach { i => 
        screenHandler.println("\t" + i)
      }
    }
  }
}
