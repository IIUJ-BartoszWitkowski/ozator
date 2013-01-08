package ozer.handlers

import ozer.Globals

class TagHandlerImpl(
    screenHandler: ScreenHandler,
    fileSystemHandler: FilesystemHandler,
    dbHandler: DbHandler)
  extends TagHandler 
  with Globals {

  val separator = fileSystemHandler.separator
  
  def wasDbCreated() = dbHandler.wasDbCreated()

  def tagDir() = dbHandler.ozerDb() map { 
    _ + fileSystemHandler.separator + Directories.Tags
  }

  def maybeCreateCathegoryDir(cathegoryName: String): Option[String] = {
    assert(wasDbCreated()) 

    val dir = tagDir().get + separator + cathegoryName

    if (!existsCathegory(cathegoryName)) {
      val create = screenHandler.ynChoice("Create new cathegory: '" + cathegoryName + "'?")
      if (!create) return None

      fileSystemHandler.mkDir(dir)
    }

    Some(dir)
  }

  def maybeCreateTagDir(cathegoryName: String, tagName: String): Option[String] = {
    val dir = tagDir().get + separator + cathegoryName + separator + tagName

    if (!existsTag(cathegoryName, tagName)) {
      val create = screenHandler.ynChoice("Create new tag '" + tagName + "'in cathegory '" + cathegoryName + "'?")
      if (!create) return None

      fileSystemHandler.mkDir(dir)
    }

    Some(dir)
  }

  override def addTagToFile(cathegoryName: String, tagName: String, fileRelative: String) {
    if (!isLegal(cathegoryName)) {
      screenHandler.println("Illegal cathegory name: '" + cathegoryName + "'")
      return
    }
    
    if (!wasDbCreated) { 
      screenHandler.println(Errors.OzerDbNotSetUp)
      return
    }

    val file = fileSystemHandler.expand(fileRelative)

    if (!dbHandler.exists(file)) {
      screenHandler.println("File: '" + file + "' can't be found in ozer data base. " +
        "Was the source directory added by `ozer source add DIR'? " +
        "Was the ozer database updated " +
        "(Run `ozer db status' and/or `ozer db update')?")

      return
    }

    assertTagDirExists()

    val cathegoryDir = maybeCreateCathegoryDir(cathegoryName)
    if (!cathegoryDir.isDefined) return

    val tagDir = maybeCreateTagDir(cathegoryName, tagName)
    if (!tagDir.isDefined) return

    val name = fileSystemHandler.basename(file)
    val targetPath = dbHandler.getDbPath(file)
    val linkPath = cathegoryDir + separator + tagName

    fileSystemHandler.link(targetPath, linkPath)
  }

  def isLegal(name: String) = {
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
