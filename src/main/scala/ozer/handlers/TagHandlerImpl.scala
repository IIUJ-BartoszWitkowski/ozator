package ozer.handlers

import ozer.Globals

class TagHandlerImpl(
    screenHandler: ScreenHandler,
    fileSystemHandler: FilesystemHandler,
    movieHandler: MovieHandler,
    dbHandler: DbHandler)
  extends TagHandler 
  with Globals {

  val separator = fileSystemHandler.separator
  
  def wasDbCreated() = dbHandler.wasDbCreated()

  def tagDir() = dbHandler.ozerDb() map { 
    _ + fileSystemHandler.separator + Directories.Tags
  }

  def maybeCreateCathegoryDir(cathegoryName: String, force: Boolean = false): Option[String] = {
    assert(wasDbCreated()) 

    val dir = tagDir().get + separator + cathegoryName

    if (!existsCathegory(cathegoryName)) {
      val create = force || screenHandler.ynChoice("Create new cathegory: '" + cathegoryName + "'?")
      if (!create) return None

      fileSystemHandler.mkDir(dir)
    }

    Some(dir)
  }

  def maybeCreateTagDir(cathegoryName: String, tagName: String, force: Boolean = false): Option[String] = {
    val dir = tagDir().get + separator + cathegoryName + separator + tagName

    if (!existsTag(cathegoryName, tagName)) {
      val create = force || 
        screenHandler.ynChoice("Create new tag '" + tagName + "' in cathegory '" + cathegoryName + "'?")
      if (!create) return None

      fileSystemHandler.mkDir(dir)
    }

    Some(dir)
  }

  def addTagToPath(
      cathegoryName: String, 
      tagName: String, 
      exists: => Boolean,
      notExistsMessage: => String,
      path: => String, 
      targetPath: => String,
      force: => Boolean = false) {

    if (isIllegal(cathegoryName)) {
      screenHandler.println("Illegal cathegory name: '" + cathegoryName + "'")
      return
    }
    if (isIllegal(tagName)) {
      screenHandler.println("Illegal tag name: '" + tagName + "'")
      return
    }
    
    if (!wasDbCreated()) { 
      screenHandler.println(Errors.OzerDbNotSetUp)
      return
    }

    if (!exists) {
      screenHandler.println(notExistsMessage)

      return
    }

    assertTagDirExists()

    val cathegoryDir = maybeCreateCathegoryDir(cathegoryName, force)
    if (!cathegoryDir.isDefined) return

    val tagDir = maybeCreateTagDir(cathegoryName, tagName, force)
    if (!tagDir.isDefined) return

    val name = fileSystemHandler.basename(path)
    val linkPath = tagDir.get + separator + name

    if (fileSystemHandler.exists(linkPath)) {
      screenHandler.println("Warning: tag '" + tagName +"' in cathegory '" + cathegoryName + "' exists " +
        "for: " + path + " (skipping).")
      return
    }

    fileSystemHandler.link(targetPath, linkPath)
  }

  override def addTagToFile(cathegoryName: String, tagName: String, fileRelative: String) {
    lazy val path = fileSystemHandler.expand(fileRelative)
    lazy val exists = dbHandler.existsInDb(path)
    lazy val notExistsMessage =
        "File: '" + path + "' can't be found in ozer data base. " +
        "Was the source directory added by `ozer source add DIR'? " +
        "Was the ozer database updated " +
        "(Run `ozer db status' and/or `ozer db update')?"
    lazy val targetPath = dbHandler.dbPath(path)
    addTagToPath(cathegoryName, tagName, exists, notExistsMessage, path, targetPath)
  }

  override def addTagToMovie(cathegoryName: String, tagName: String, title: String, force: Boolean = false) {
    lazy val exists = movieHandler.exists(title)
    lazy val notExistsMessage =
        "Movie: '" + title + "' can't be found in ozer data base. " +
        "Was the movie added by `ozer autotag'? " +
        "Was the ozer database updated " +
        "(Run `ozer ls untagged' and/or `ozer autotag')?"
    lazy val path = movieHandler.moviePath(title)
    lazy val targetPath = path
    addTagToPath(cathegoryName, tagName, exists, notExistsMessage, path, targetPath, force)
  }

  override def removeTagFromFile(cathegoryName: String, tagName: String, fileRelative: String) {
    if (!existsCathegory(cathegoryName)) {
      screenHandler.println("Error cathegory '" + cathegoryName + "' doesn't exist.")
      return
    }

    if (!existsTag(cathegoryName, tagName)) {
      screenHandler.println("Error no tag '" + tagName + "' in cathegory '" + cathegoryName + "'.")
      return
    }

    val dir = dirFromCathegoryAndTag(cathegoryName, tagName)   
    println(dir)
    val fileName = fileSystemHandler.basename(fileRelative)

    val linkPath = dir + separator + fileName
    if (fileSystemHandler.exists(linkPath)) {
      val remove = screenHandler.ynChoice("Remove tag '" + tagName + "' from cathegory '" + cathegoryName + 
        "' for file '" + fileRelative + "'?")

      if (remove) {
        fileSystemHandler.rm(linkPath)
      }
    } else {
      screenHandler.println("Error no tag '" + tagName + "' in cathegory '" + cathegoryName + 
        "' for file '" + fileRelative)
    }
  }

  override def removeAllTagsInCathegoryFromFile(cathegoryName: String, fileRelative: String) {
    if (!existsCathegory(cathegoryName)) {
      screenHandler.println("Error cathegory '" + cathegoryName + "' doesn't exist.")
      return
    }

    val dbPath = dbHandler.dbPath(fileRelative)

    val tags = for {
      tag <- tagsInCathegory(cathegoryName)
      dir = dirFromCathegoryAndTag(cathegoryName, tag)
      fileName <- fileSystemHandler.ls(dir)
      linkPath = dir + separator + fileName
      if (fileSystemHandler.areSame(linkPath, dbPath))
    } yield (tag, linkPath)
  
    if (tags.isEmpty) {
      screenHandler.println("Error no tags in cathegory '" + cathegoryName + "' for file '" + fileRelative + ".")
    } else {
      for ((tag, file) <- tags) {
        val remove = screenHandler.ynChoice("Remove tag '" + tag + "' from file '" + fileRelative + "'?")
        if (remove) {
          fileSystemHandler.rm(file)
        }
      }
    }
  }

  override def tagsOfMovie(title: String): Seq[(String, String)] = {
    if (wasDbCreated()) {
      val moviePath = movieHandler.moviePath(title)

      for {
        cathegory <- cathegories()
        tag <- tagsInCathegory(cathegory) 
        dir = dirFromCathegoryAndTag(cathegory, tag)
        fileName <- fileSystemHandler.ls(dir)
        if (fileSystemHandler.areSame(dir + separator + fileName, moviePath))
      } yield (cathegory, tag)
    } else {
      List.empty[(String, String)]
    }
  }

  override def tagsOfFile(fileRelative: String): Seq[(String, String)] = {
    if (wasDbCreated()) {
      val dbPath = dbHandler.dbPath(fileRelative)

      for {
        cathegory <- cathegories()
        tag <- tagsInCathegory(cathegory) 
        dir = dirFromCathegoryAndTag(cathegory, tag)
        fileName <- fileSystemHandler.ls(dir)
        if (fileSystemHandler.areSame(dir + separator + fileName, dbPath))
      } yield (cathegory, tag)
    } else {
      List.empty[(String, String)]
    }
  }

  override def dirFromCathegory(cathegory: String): String = {
    tagDir().get + separator + cathegory 
  }

  override def dirFromCathegoryAndTag(cathegory: String, tag: String): String = {
    dirFromCathegory(cathegory) + separator + tag
  }

  def isIllegal(name: String) = {
    name.contains("\0") || name.contains("/")
  }
  
  def assertTagDirExists() {
    val dir = tagDir().get 
    if (!fileSystemHandler.exists(dir)) {
      fileSystemHandler.mkDir(dir)
      assert(fileSystemHandler.exists(dir))
    }
  }

  override def existsCathegory(cathegory: String): Boolean = {
    cathegories contains cathegory 
  }

  override def existsTag(cathegory: String, tag: String): Boolean = {
    tagsInCathegory(cathegory) contains tag
  }

  override def cathegories(): Seq[String] = {
    if (wasDbCreated) fileSystemHandler.ls(tagDir.get)
    else              List.empty[String]
  }

  override def tagsInCathegory(cathegory: String): Seq[String] = {
    if (wasDbCreated) fileSystemHandler.ls(tagDir.get + separator + cathegory)
    else              List.empty[String]
  }
}
