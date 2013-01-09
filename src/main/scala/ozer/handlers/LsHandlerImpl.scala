package ozer.handlers

import ozer.Globals

class LsHandlerImpl(
    screenHandler: ScreenHandler,
    fileSystemHandler: FilesystemHandler,
    dbHandler: DbHandler,
    movieHandler: MovieHandler,
    tagHandler: TagHandler)
  extends LsHandler 
  with Globals {

  override def list(): Seq[String] = {
    if (dbHandler.wasDbCreated()) fileSystemHandler.ls(dbHandler.allDir().get)
    else                          List.empty[String]
  }

  override def listFullPath(): Seq[String] = {
    list() map { dbHandler.allDir().get + fileSystemHandler.separator + _ }
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

  def listNotUpdated(): Seq[String] = {
    val allInDb = listFullPath()
    val allMovies = movieHandler.listFullPath() map (fileSystemHandler.readLink(_))

    val notUpdated = allInDb.filter { file =>
       !allMovies.contains(file)
    }

    notUpdated
  }

  override def tagsOfFile(file: String): Seq[(String, String)] = {
    tagHandler.tagsOfFile(file)
  }

  override def printTagsOfFile(file: String) {
    if (dbHandler.existsInDb(file)) {
      for ((cathegory, tag) <- tagsOfFile(file)) {
        screenHandler.println(cathegory + " : " + tag)
      }
    } else {
      screenHandler.println("Error - file '" + file + "' doesn't exist in ozer db.")
    }
  }
}
