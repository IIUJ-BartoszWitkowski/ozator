package ozer.util

import java.util.{Map => JMap}
import java.io.File
import org.ini4j.{Ini => JIni, Wini, MultiMap}
import scala.collection.JavaConverters._

object Inis {
  type Sections = Map[String, List[String]]
  type Ini = Map[String, Sections]

  private[this] def toIni(jIni: JIni): Ini = {
    val mapOfSections = jIni.asScala.toMap

    mapOfSections.map { case (sectionName, iniSection) => 
      val keySet = iniSection.keySet.asScala
      val listOfTuples: List[(String, List[String])] = for (key <- keySet.toList) yield {
        (key, iniSection.getAll(key).asScala.toList)
      }
      (sectionName, Map(listOfTuples: _*))
    }
  }

  def fromFile(fileName: String): Ini = {
    val file = new File(fileName)
    fromFile(file)
  }

  def fromFile(file: File): Ini = {
    try {
      val jIni = new JIni(file)
      toIni(jIni)
    } catch {
      case _ => Map.empty
    }
  }

  def toFile(ini: Ini, fileName: String): Unit = {
    toFile(ini, new File(fileName))
  }

  def toFile(ini: Ini, file: File): Unit = {
    if (!file.exists) file.createNewFile

    val jIni = new JIni(file)
    jIni.clear()

    for {
      (sectionName, properites) <- ini
      (key, values) <- properites
      value <- values
    } {
      ///println(sectionName + ":" + value)
      val sectionOpt: Option[MultiMap[String, String]] = Option(jIni.get(sectionName))
      sectionOpt match {
        case Some(section) => section.add(key, value)
        case None =>          jIni.put(sectionName, key, value)
      }
    }

    jIni.store
  }
}
