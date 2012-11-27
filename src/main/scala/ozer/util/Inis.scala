package ozer.util

import java.util.{Map => JMap}
import java.io.File
import org.ini4j.{Ini => JIni, Wini}
import scala.collection.JavaConverters._

object Inis {
  type Sections = Map[String, String]
  type Ini = Map[String, Sections]

  private[this] def toIni(jIni: JIni): Ini = {
    jIni.asScala.toMap.map {
      case (key, value) => (key, value.asScala.toMap)
    }
  }

  def fromFile(fileName: String): Ini = {
    val file = new File(fileName)
    fromFile(file)
  }

  def fromFile(file: File): Ini = {
    val jIni = new JIni(file)
    toIni(jIni)
  }

  def toFile(ini: Ini, fileName: String): Unit = {
    toFile(ini, new File(fileName))
  }

  def toFile(ini: Ini, file: File): Unit = {
    val wini = new Wini(file)

    for {
      (sectionName, properites) <- ini
      (key, value) <- properites
    } wini.put(sectionName, key, value)

    wini.store
  }
}
