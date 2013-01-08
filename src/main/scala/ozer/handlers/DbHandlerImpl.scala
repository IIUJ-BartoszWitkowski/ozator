package ozer.handlers

import ozer.Globals
import ozer.util.Inis.Ini

object PureDbHandler extends Globals {
  def ozerDb(config: Ini): Option[String] = {
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

  def writeDirToConfig(dir: String) = {
    write(Config.DbSection, Config.DbDir, List(dir))
  }

  def ozerDb(): Option[String] = PureDbHandler.ozerDb(ini)
  def allDir(): Option[String] = ozerDb() map {
    _ +  fileSystemHandler.separator + Directories.All
  }
  def wasDbCreated = ozerDb.isDefined

  def create(dirNameRelative: String): Unit = {
    val dirName = fileSystemHandler.expand(dirNameRelative) 
    val exists = fileSystemHandler.exists(dirName)
    val (canCreate, warning) = PureDbHandler.canCreate(ozerDb(), exists)

    if (canCreate) {
      fileSystemHandler.mkDir(dirName)
      val allDir = dirName +  fileSystemHandler.separator + Directories.All
      fileSystemHandler.mkDir(allDir)

      val sources = sourceHandler.list()

      for {
        source <- sources
        item <- fileSystemHandler.ls(source)
      } {
        val linkPath = allDir + fileSystemHandler.separator + fileSystemHandler.basename(item)
        val itemPath = source + fileSystemHandler.separator + item
        fileSystemHandler.link(itemPath, linkPath)
      }

      writeDirToConfig(dirName)
    } else {
      screenHandler.println(warning.get)
    }
  }
  
  def link(source: String, item: String, dirName: String): Unit = {
    val linkPath = dirName + fileSystemHandler.separator + fileSystemHandler.basename(item)
    val itemPath = source + fileSystemHandler.separator + item
    fileSystemHandler.link(itemPath, linkPath)
  }

  def contains(dir: String, item: String): Boolean = {
    val links = fileSystemHandler.ls(dir)
    links.foreach { link =>
      val linkPath = dir + fileSystemHandler.separator + link
        if (fileSystemHandler.areSame(item, linkPath)) return true
    }

    false
  }

  def listNotUpdated: List[(String, String)] = {
    if (allDir().isDefined) {
      val sources = sourceHandler.list()

      for {
        source <- sources
        item <- fileSystemHandler.ls(source)
        if (!contains(allDir().get, source + fileSystemHandler.separator + item))
      } yield (source, item)
    } else {
      List.empty[(String, String)]
    }
  }

  def update(): Unit = {
    if (!wasDbCreated) {
      screenHandler.println("Error - db not created (use `ozer db create DIR` to create ozer db)")
      return 
    }

    val notUpdated = listNotUpdated

    for {
      (source, item) <- listNotUpdated
    } link(source, item, allDir().get)
  }
  
  def status(): Unit = {
    if (!wasDbCreated) {
      screenHandler.println("Error - db not created (use `ozer db create DIR` to create ozer db)")
      return 
    }

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

  def exists(pathRelative: String): Boolean = {
    if (!wasDbCreated) {
      return false
    }

    val path = fileSystemHandler.expand(pathRelative) 
    val fileName = fileSystemHandler.basename(pathRelative)
    val potentialLink = allDir().get + fileSystemHandler.separator + fileName
    
    fileSystemHandler.areSame(path, potentialLink)
  }
}
