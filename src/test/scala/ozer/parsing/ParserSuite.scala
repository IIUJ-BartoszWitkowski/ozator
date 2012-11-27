package ozer.parsing

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParserSuite extends FunSuite {
  val parser = new Parser
  
  test("source") {
    assert(parser.parseArguments(List("source")) == Error(parser.SourceHelp()))
  }
  
  test("source ls") {
    assert(parser.parseArguments(List("source", "ls")) == Source.List)
  }
  
  test("source ls ?") {
    assert(parser.parseArguments(List("source", "ls", "argument")) == Error(parser.SourceHelp.listHelp))
  }
  
  test("source add") {
    assert(parser.parseArguments(List("source", "add")) == Error(parser.SourceHelp.addHelp))
  }
  
  test("source add files") {
     assert(parser.parseArguments(List("source", "add", "file1", "file2")) == Source.Add(List("file1", "file2")))
  }
  
  test("source rm") {
     assert(parser.parseArguments(List("source", "rm")) == Error(parser.SourceHelp.rmHelp))
  }
  
  test("source rm files") {
     assert(parser.parseArguments(List("source", "rm", "file1", "file2")) == Source.Rm(List("file1", "file2")))
  }

  test("db create") {
     assert(parser.parseArguments(List("db", "create")) == Error(parser.DbHelp.createHelp))
  }

  test("db create dir") {
     assert(parser.parseArguments(List("db", "create", "dir")) == Db.Create("dir"))
  }

  test("db create dir ?") {
     assert(parser.parseArguments(List("db", "create", "dir", "dir2")) == Error(parser.DbHelp.createHelp))
  }

  test("db update") {
     assert(parser.parseArguments(List("db", "update")) == Db.Update)
  }

  
  test("db update ?") {
     assert(parser.parseArguments(List("db", "update", "anything")) == Error(parser.DbHelp.updateHelp))
  }

  test("db status") {
     assert(parser.parseArguments(List("db", "status")) == Db.Status)
  }

  test("db status ?") {
     assert(parser.parseArguments(List("db", "status", "anything")) == Error(parser.DbHelp.statusHelp))
  }
  
  def assertp(s: String) = println(s)
}
