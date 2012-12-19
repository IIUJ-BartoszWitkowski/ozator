package ozer.handlers

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import ozer.Globals
import ozer.util.Inis.Ini

@RunWith(classOf[JUnitRunner])
class PureDbHandlerSuite extends FunSuite with Globals {
  test("dir - without section gives None") {
    val config = Map.empty[String, Map[String, List[String]]]
    assert(PureDbHandler.dir(config) == None)
  }

  test("dir - without empty section gives None") {
    val config = Map(Config.DbSection -> Map.empty[String, List[String]])
    assert(PureDbHandler.dir(config) == None)
  }

  test("dir - happy path") {
    val config: Ini = Map(
      Config.DbSection -> Map(
        Config.DbDir -> List("db-dir")))
    assert(PureDbHandler.dir(config) == Some("db-dir"))
  }
}
