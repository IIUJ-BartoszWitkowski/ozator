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

  object Config {
    lazy val FileName = "config.ini"
    lazy val SourceSection = "sources"
    lazy val SourceDir = "dirs"
    lazy val DbSection = "db"
    lazy val DbDir = "dir"
  }

  def osNotSupported = throw new RuntimeException(
    "os " + System.getProperty("os.name") + " not supported")
}
