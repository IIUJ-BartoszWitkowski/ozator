package ozer.util

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class InisSuite extends FunSuite {
  import Inis._ 

  test("Highlevel integration test") {
    val givenIni: Ini = Map(
      "section1" -> Map(
        "property1" -> List("value1"),
        "property2" -> List("value2")),
      "section2" -> Map(
        "property3" -> List("value3"),
        "property4" -> List("value4", "value5")))

    val fileName = "test.ini"

    Inis.toFile(givenIni, fileName)

    val readIni = Inis.fromFile(fileName)

    //println(readIni)
    assert(givenIni == readIni)
  }
}
