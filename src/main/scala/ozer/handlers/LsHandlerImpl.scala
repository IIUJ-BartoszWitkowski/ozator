package ozer.handlers

import ozer.Globals

class LsHandlerImpl(
    screenHandler: ScreenHandler,
    fileSystemHandler: FilesystemHandler,
    dbHandler: DbHandler,
    tagHandler: TagHandler)
  extends LsHandler 
  with Globals {

  override def list(): Seq[String] = {
    if (dbHandler.wasDbCreated()) fileSystemHandler.ls(dbHandler.allDir().get)
    else                          List.empty[String]
  }

  override def printList() {
    if (!dbHandler.wasDbCreated()) {
      screenHandler.println(Errors.OzerDbNotSetUp)
      return
    }

    for (item <- list()) {
      screenHandler.println(item)
    }
  }

  override def tagsOfFile(file: String): Seq[(String, String)] = {
    tagHandler.tagsOfFile(file)
  }

  override def printTagsOfFile(file: String) {
    for ((cathegory, tag) <- tagsOfFile(file)) {
      screenHandler.println(cathegory + " : " + tag)
    }
  }
}
