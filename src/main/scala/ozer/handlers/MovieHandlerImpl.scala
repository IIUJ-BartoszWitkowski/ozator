package ozer.handlers

import scala.collection.mutable.StringBuilder
import scala.collection.mutable.Buffer
import ozer.util.ImdbData
import ozer.Globals
import ozer.data._

class MovieHandlerImpl (
    screenHandler: ScreenHandler,
    dbHandler: DbHandler,
    fileSystemHandler: FilesystemHandler)
  extends MovieHandler 
  with Globals {

  def nameDir() = dbHandler.ozerDb() map {
    _ + fileSystemHandler.separator + "by-name"
  }

  def wasCreated() = nameDir().isDefined

  def exists(name: String): Boolean = {
    if (!wasCreated()) false
    else             fileSystemHandler.exists(moviePath(name))
  }

  def moviePath(name: String, create: Boolean = false): String = {
    val dir = nameDir().get
    if (create && !fileSystemHandler.exists(dir)) {
      fileSystemHandler.mkDir(dir)
      assert(fileSystemHandler.exists(dir))
    } 
    val firstLetter = {
      val c = name(0).toUpper
      if (c.isUpper || c.isDigit) c else '_'
    }
    val sep = fileSystemHandler.separator
    val letterDir = dir + sep + firstLetter

    if (create && !fileSystemHandler.exists(letterDir)) {
      fileSystemHandler.mkDir(letterDir)
      assert(fileSystemHandler.exists(letterDir))
    }
    
    letterDir + sep + name
  }

  def listNames(): Seq[String] = {
    if (wasCreated()) {
      val dir = nameDir().get
      fileSystemHandler.ls(dir) flatMap { letterDir =>
        fileSystemHandler.ls(dir + fileSystemHandler.separator + letterDir)
      }
    } else {
      List.empty[String]
    }
  }

  def listFullPath(): Seq[String] = listNames() map (moviePath(_))

  override def createMovies(fileNames: Seq[String]): Seq[Movie] = {
    val movies = Buffer.empty[Movie]

    fileNames foreach { fileName =>
      val propositions = ImdbData.getBestPropositions(fileName, Cache)
      val builder = new StringBuilder()

      val size = propositions.size

      builder ++= "Which is the most apropriate for file: `"
      builder ++= fileName 
      builder ++= "`?\n"
      for (i <- 1 to size) {
        builder ++= i + ") " + propositions(i - 1).title + "\n"
      }
      builder ++= size + 1 + "] Manual input. \n"
      builder ++= size + 2 + "] skip"

      val choice = screenHandler.nChoice(builder.toString, size + 2) - 1
      if (choice < size) {
        val movie = propositions(choice)
        val name = movie.title

        val targetPath = dbHandler.allDir().get + fileSystemHandler.separator + fileName
        val linkPath = moviePath(name, create = true)

        fileSystemHandler.link(targetPath, linkPath)
        movies += movie
      } else if (choice == size) {
        var getData = true

        var title = ""
        var year = ""

        while (getData) {
          title = screenHandler.getString("The movie title (as shown in the internet movie database imdb)?")
          year = screenHandler.getNumeric(
            "The movie production year (as shown in the internet movie database?", 1800, 2200)

          screenHandler.println("")
          screenHandler.println("title: `" + title + "`")
          screenHandler.println("year: `" + year + "`")
          getData = !screenHandler.ynChoice("Is this correct?")
        }

        val targetPath = dbHandler.allDir().get + fileSystemHandler.separator + fileName
        val linkPath = moviePath(title, create = true)
        fileSystemHandler.link(targetPath, linkPath)

        movies += Movie(title, year)
      }
    }
    movies
  }

}
