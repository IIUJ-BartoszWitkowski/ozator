package ozer.handlers

import ozer.Globals
import ozer.util.ImdbData
import ozer.data._

class AutotagHandlerImpl(
    screenHandler: ScreenHandler,
    fileSystemHandler: FilesystemHandler,
    lsHandler: LsHandler,
    movieHandler: MovieHandler,
    tagHandler: TagHandler) 
  extends AutotagHandler 
  with Globals {

  def autotag() {
    val fileNames = lsHandler.listNotUpdated() map { fileSystemHandler.basename(_) }
    val movies = movieHandler.createMovies(fileNames)
    movies foreach { movie =>
      val name = movie.title

      tagHandler.addTagToMovie("year", movie.year, name, true)

      val genres = ImdbData.getGenres(GenresFile, name)
      genres foreach { tagHandler.addTagToMovie("genre", _, name, true) }

      val ratings = ImdbData.getRating(RatingsFile, name)
      ratings foreach { tagHandler.addTagToMovie("rating", _, name, true) }
      
      val directors = ImdbData.getDirectors(DirectorsFile, name)
      directors foreach { tagHandler.addTagToMovie("director", _, name, true) }
    }

    screenHandler.println("Creating tags - this may take a while.")

    val names = movies.map(_.title)
    
    val actorsMap = ImdbData.getActorsAndActresses(ActorsFile, ActressesFile, names)
    names.foreach { name =>
      val actors = actorsMap.getOrElse(name, List.empty[Actor])
      val best = actors.take(10) map (_.name)
      best foreach { tagHandler.addTagToMovie("actor", _, name, true) }
    }
  }
}
