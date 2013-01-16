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

  test("tag auto") {
    val args = List("tag", "auto")
    assert(parser.parseArguments(args) == Tag.Auto)
  }

  test("tag auto ?") {
    val args = List("tag", "auto", "any")
    assert(parser.parseArguments(args) == Error(parser.TagHelp.autoHelp))
  }

  test("tag repair") {
    val args = List("tag", "repair")
    assert(parser.parseArguments(args) == Error(parser.TagHelp.repairHelp))
  }
  
  test("tag repair title") {
    val args = List("tag", "repair", "title")
    assert(parser.parseArguments(args) == Tag.RepairAllTagsInMovie("title"))
  }

  test("tag repair tag title") {
    val args = List("tag", "repair", "tag", "title")
    assert(parser.parseArguments(args) == Tag.RepairTagInMovie("tag", "title"))
  }

  test("tag repair tag title ?") {
    val args = List("tag", "repair", "tag", "title", "anything")
    assert(parser.parseArguments(args) == Error(parser.TagHelp.repairHelp))
  }

  test("tag rm") {
    val args = List("tag", "rm")
    assert(parser.parseArguments(args) == Error(parser.TagHelp.rmHelp))
  }

  test("tag rm tag # from what?") {
    val args = List("tag", "rm", "tag") 
    assert(parser.parseArguments(args) == Error(parser.TagHelp.rmHelp))
  }

  test("tag rm tag movie") {
    val args = List("tag", "rm", "tag", "movie") 
    assert(parser.parseArguments(args) == Tag.RmAllTagsFromMovie("tag", "movie"))
  }

  test("tag rm key value movie") {
    val args = List("tag", "rm", "key", "value", "movie") 
    assert(parser.parseArguments(args) == Tag.RmTagFromMovie("key", "value", "movie"))
  }

  test("tag rm key value movie ?") {
    val args = List("tag", "rm", "key", "value", "movie", "any") 
    assert(parser.parseArguments(args) == Error(parser.TagHelp.rmHelp))
  }

  test("tag rm key -- file") {
    val args = List("tag", "rm", "key", "--", "file") 
    assert(parser.parseArguments(args) == Tag.RmAllTagsFromFile("key", "file"))
  }

  test("tag rm key value -- file") {
    val args = List("tag", "rm", "key", "value", "--", "file") 
    assert(parser.parseArguments(args) == Tag.RmTagFromFile("key", "value", "file"))
  }

  test("tag rm key value -- file ?") {
    val args = List("tag", "rm", "key", "value", "--", "file", "any") 
    assert(parser.parseArguments(args) == Error(parser.TagHelp.rmHelp))
  }

  test("tag add") {
    val args = List("tag", "add")
    assert(parser.parseArguments(args) == Error(parser.TagHelp.addHelp))
  }

  test("tag add key # value? title?") {
    val args = List("tag", "add", "key")
    assert(parser.parseArguments(args) == Error(parser.TagHelp.addHelp))
  }

  test("tag add key value # title?") {
    val args = List("tag", "add", "key", "value")
    assert(parser.parseArguments(args) == Error(parser.TagHelp.addHelp))
  }

  test("tag add key value title") {
    val args = List("tag", "add", "key", "value", "title")
    assert(parser.parseArguments(args) == Tag.AddTagToMovie("key", "value", "title"))
  }

  test("tag add key value title ?") {
    val args = List("tag", "add", "key", "value", "title", "any")
    assert(parser.parseArguments(args) == Error(parser.TagHelp.addHelp))
  }

  test("tag add key value -- file") {
    val args = List("tag", "add", "key", "value", "--", "file")
    assert(parser.parseArguments(args) == Tag.AddTagToFile("key", "value", "file"))
  }

  test("tag add key value -- file ?") {
    val args = List("tag", "add", "key", "value", "--", "file", "any")
    assert(parser.parseArguments(args) == Error(parser.TagHelp.addHelp))
  } 

  test("find-duplicates") {
    val args = List("find-duplicates")
    assert(parser.parseArguments(args) == FindDuplicates)
  }

  test("find-duplicates ?") {
    val args = List("find-duplicates", "any")
    assert(parser.parseArguments(args) == Error(parser.FindDuplicatesHelp()))
  }

  test("grep") {
    val args = List("grep")
    assert(parser.parseArguments(args) == Error(parser.GrepHelp()))
  }

  test("grep tag # patternA patternB?") {
    val args = List("grep", "tag")
    assert(parser.parseArguments(args) == Error(parser.GrepHelp()))
  }
  
  test("grep tag patternA # patternB?") {
    val args = List("grep", "tag", "patternA")
    assert(parser.parseArguments(args) == Error(parser.GrepHelp()))
  }

  test("grep tag a b") {
    val args = List("grep", "tag", "patternA", "patternB")
    assert(parser.parseArguments(args) == Grep("tag", "patternA", "patternB"))
  }

  test("grep tag a b ?") {
    val args = List("grep", "tag", "pattern", "pattern", "anything")
    assert(parser.parseArguments(args) == Error(parser.GrepHelp()))
  }

  test("ls") {
    val args = List("ls")
    assert(parser.parseArguments(args) == Ls.Everything)
  }

  test("ls ?") {
    val args = List("ls", "lala")
    assert(parser.parseArguments(args) == Error(parser.LsHelp()))
  }

  test("ls untagged") {
    val args = List("ls", "untagged")
    assert(parser.parseArguments(args) == Ls.Untagged)
  }

  test("ls untagged ?") {
    val args = List("ls", "untagged", "blah")
    assert(parser.parseArguments(args) == Error(parser.LsHelp.untaggedHelp))
  }

  test("ls tag title") {
    val args = List("ls", "tag", "title")
    assert(parser.parseArguments(args) == Ls.TagsOfMovie("title"))
  }

  test("ls tag title ?") {
    val args = List("ls", "tag", "title", "asdf")
    assert(parser.parseArguments(args) == Error(parser.LsHelp.tagHelp))
  }

  test("ls tag -- file") {
    val args = List("ls", "tag", "--", "file")
    assert(parser.parseArguments(args) == Ls.TagsOfFile("file"))
  }

  test("ls tag -- file ??") {
    val args = List("ls", "tag", "--", "file", "any")
    assert(parser.parseArguments(args) == Error(parser.LsHelp.tagHelp))
  }

  test("rm-file") {
    val args = List("rm-file")
    assert(parser.parseArguments(args) == Error(parser.RmFileHelp()))
  }

  test("rm-file file1 file2") {
    val args = List("rm-file", "file1", "file2")
    assert(parser.parseArguments(args) == Rm.Files(List("file1", "file2")))
  }

  test("rm-movie") {
    val args = List("rm-movie")
    assert(parser.parseArguments(args) == Error(parser.RmMovieHelp()))
  }

  test("rm-movie title1 title2") {
    val args = List("rm-movie", "title1", "title2")
    assert(parser.parseArguments(args) == Rm.Movies(List("title1", "title2")))
  }

  test("rm-movie -tag key value") {
    val args = List("rm-movie", "-tag", "key", "value")
    assert(parser.parseArguments(args) == Rm.MovieByKeyValue("key", "value"))
  }

  test("rm-movie -tag key Zvalue ?") {
    val args = List("rm-movie", "-tag", "key", "value", "any")
    assert(parser.parseArguments(args) == Error(parser.RmMovieHelp()))
  }

  test("mv-movie") {
    val args = List("mv-movie")
    assert(parser.parseArguments(args) == Error(parser.MoveMovieHelp()))
  }

  test("mv-movie source") {
    val args = List("mv-movie", "title")
    assert(parser.parseArguments(args) == Error(parser.MoveMovieHelp()))
  }

  test("mv-movie source dest") {
    val args = List("mv-movie", "title", "dest")
    assert(parser.parseArguments(args) == Move.Movie("title", "dest"))
  }

  test("mv-movie source dest ?") {
    val args = List("mv-movie", "title", "dest", "any")
    assert(parser.parseArguments(args) == Error(parser.MoveMovieHelp()))
  }

  test("mv-file") {
    val args = List("mv-file")
    assert(parser.parseArguments(args) == Error(parser.MoveFileHelp()))
  }

  test("mv-file source") {
    val args = List("mv-file", "source")
    assert(parser.parseArguments(args) == Error(parser.MoveFileHelp()))
  }

  test("mv-file source dest") {
    val args = List("mv-file", "source", "dest")
    assert(parser.parseArguments(args) == Move.File("source", "dest"))
  }

  test("mv-file source dest ?") {
    val args = List("mv-file", "source", "dest", "any")
    assert(parser.parseArguments(args) == Error(parser.MoveFileHelp()))
  }

  test("mv-all") {
    val args = List("mv-all")
    assert(parser.parseArguments(args) == Error(parser.MoveAllHelp()))
  }

  test("mv-all dest") {
    val args = List("mv-all", "dest")
    assert(parser.parseArguments(args) == Move.All("dest"))
  }

  test("mv-all dest ?") {
    val args = List("mv-all", "dest", "asdf")
    assert(parser.parseArguments(args) == Error(parser.MoveAllHelp()))
  }

  test("xyzzy") {
     assert(parser.parseArguments(List("xyzzy")) == Error(parser.CommandHelp()))
  }

  test("readlink file") {
    val args = List("readlink", "title")
    assert(parser.parseArguments(args) == ReadLink.File("title"))
  }

  test("readlink file ?") {
    val args = List("readlink", "title", "any")
    assert(parser.parseArguments(args) == Error(parser.ReadLinkHelp()))
  }

  def assertp(s: String) = println(s)
}
