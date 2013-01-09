package ozer

import java.io.File

trait Globals {
  sealed abstract trait Os 
  case object Linux extends Os
  case object Windows extends Os
  case object Other extends Os

  lazy val os: Os = {
    val osName = System.getProperty("os.name").toLowerCase

    if (osName startsWith "linux")     Linux
    else if (osName startsWith "win")  Windows
    else                               Other
  }

  lazy val OzerHome: String = {
    val ozerDir = "ozer"
    os match {
      case Linux =>    System.getProperty("user.home") + File.separator + "." + ozerDir
      case Windows =>  osNotSupported // System.getProperty("user.home") + File.separator       + ozerDir
      case Other =>    osNotSupported
    }
  }

  lazy val Cache = OzerHome + File.separator + "cache"
  lazy val GenresFile = OzerHome + File.separator + "genres.utf8"
  lazy val RatingsFile = OzerHome + File.separator + "ratings.utf8"
  lazy val DirectorsFile = OzerHome + File.separator + "directors.utf8"
  lazy val ActorsFile = OzerHome + File.separator + "actors.utf8"
  lazy val ActressesFile = OzerHome + File.separator + "actresses.utf8"

  object Config {
    lazy val FileName = "config.ini"
    lazy val SourceSection = "sources"
    lazy val SourceDir = "dirs"
    lazy val DbSection = "db"
    lazy val DbDir = "dir"
  }

  object Directories {
    lazy val All = "all"
    lazy val Tags = "by-tag"
  }

  object Errors {
    lazy val OzerDbNotSetUp = 
      "Error - ozer database not created (use `ozer db create DIR` to create database)"
  }

  def osNotSupported = throw new RuntimeException(
    "os " + System.getProperty("os.name") + " not supported")
}
