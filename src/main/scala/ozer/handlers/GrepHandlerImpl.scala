package ozer.handlers

class GrepHandlerImpl(
    fileSystemHandler: FilesystemHandler,
    screenHandler: ScreenHandler,
    tagHandler: TagHandler)
  extends GrepHandler {

  override def print(cathegoryName: String, tagPattern: String, moviePattern: String) {
    if (tagHandler.existsCathegory(cathegoryName)) {
      grep(cathegoryName, tagPattern, moviePattern) foreach { case (found, movies) =>
        screenHandler.println(found + ":")
        movies.foreach { movie =>
          screenHandler.println("\t" + movie)
        }
      }
    } else {
      screenHandler.println("Cathegory name `" + cathegoryName + "' not found." +
        " Run `ozer ls cathegories` for cathegory list.")
    }
  }

  override def grep(cathegoryName: String, tagPattern: String, moviePattern: String): Seq[(String, Seq[String])] = {
    def matches(string: String, pattern: String): Boolean = {
      val regex = pattern.r
      (regex findFirstIn string).nonEmpty
    }

    if (tagHandler.existsCathegory(cathegoryName)) {
      val dir = tagHandler.dirFromCathegory(cathegoryName)
      val tags = fileSystemHandler.ls(dir)
      val matchedTags = tags filter { tag => matches(tag, tagPattern) }
      
      matchedTags map { tag =>
        val tagDir = dir + fileSystemHandler.separator + tag
        val movies = fileSystemHandler.ls(tagDir)
        val matchedMovies = movies filter { movie => matches(movie, moviePattern) }

        (tag, matchedMovies)
      }
    } else {
      List.empty
    }
  }
}
