package ozer.util

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class InisSuite extends FunSuite {
  import Inis._ 

  test("Highlevel integration test") {
    val givenIni = Map(
      "section1" -> Map(
        "property1" -> "value1",
        "property2" -> "value2"),
      "section2" -> Map(
        "property3" -> "value3",
        "property4" -> "value4"))

    val fileName = "test.ini"

    Inis.toFile(givenIni, fileName)

    val readIni = Inis.fromFile(fileName)

    println(readIni)
    assert(givenIni == readIni)
  }
}
