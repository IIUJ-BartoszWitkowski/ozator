package ozer.parsing

import ozer.parsing.{Error => OzerError}
import scala.util.parsing.combinator.RegexParsers

class Parser extends RegexParsers {
  val separator = '\0'

  trait Pretty {
    def list(s: String*) = s mkString "\n"
  }

  trait Help extends Pretty {
    def name: String
    def syntax(params: String) = app + " " + name + " " + params

    val app = "ozer"
  } 

  object SourceHelp extends Help {
    def listHelp = syntax("ls")
    def addHelp = syntax("add FILE...")
    def rmHelp = syntax("rm FILE...")

    override val name = "source"
      
    def apply() = list(listHelp, addHelp, rmHelp)
  }

  object DbHelp extends Help {
    def createHelp = syntax("create DIR")
    def updateHelp = syntax("update")
    def statusHelp = syntax("status")

    override val name = "db"

    def apply() = list(createHelp, updateHelp , statusHelp)
  }
  
  object TagHelp extends Help {
    override val name = "tag"
    def addHelp = list(
      syntax("add TAG NAME TITLE"),
      syntax("add TAG NAME -- FILE"))

    def rmHelp = list(
      syntax("rm TAG TITLE"), 
      syntax("rm TAG -- FILE"),
      syntax("rm TAG NAME TITLE"),
      syntax("rm TAG NAME -- FILE"))
    def repairHelp = list(syntax("repair TITLE"), syntax("repair TAG TITLE"))
    def autoHelp = syntax("auto")
      
    def apply() = list(
      addHelp,
      rmHelp,
      repairHelp,
      autoHelp)
  }

  object RmMovieHelp extends Help {
    override val name = "rm-movie" 
    def rmMovieHelp = syntax("TITLE...")
    def tagHelp = syntax("-tag KEY VALUE")

    def apply() = list(rmMovieHelp, tagHelp)
  } 

  object RmFileHelp extends Help {
    override val name = "rm-movie" 

    def apply() = syntax("FILE...")
  } 

  object MoveFileHelp extends Help {
    override val name = "mv-file" 

    def apply() = syntax("SOURCE DESTINATION")
  } 

  object MoveMovieHelp extends Help {
    override val name = "mv-movie" 

    def apply() = syntax("TITLE DIRECTORY")
  } 

  object MoveAllHelp extends Help {
    override val name = "mv-all" 

    def apply() = syntax("DIRECTORY")
  } 

  object LsHelp extends Help {
    override val name = "ls" 
    def untaggedHelp = syntax("untagged")
    def cathegoriesHelp = syntax("cathegories")
    def tagHelp = list(
      syntax("tag TITLE"),
      syntax("tag -- FILE"))

    def apply() = list(
      syntax(""),
      untaggedHelp,
      cathegoriesHelp,
      tagHelp)
  }

  object GrepHelp extends Help {
    override val name = "grep"

    def apply() = syntax("TAG PATTERN")
  }

  object FindDuplicatesHelp extends Help {
    override val name = "find-duplicates"

    def apply() = syntax("")
  }

  object ReadLinkHelp extends Help {
    override val name = "readlink"

    def apply() = syntax("FILE")
  }

  object CommandHelp extends Pretty {
    def apply() = list(
      SourceHelp(),
      DbHelp(),
      TagHelp(),
      RmMovieHelp(),
      RmFileHelp(),
      MoveMovieHelp(),
      MoveFileHelp(),
      MoveAllHelp(),
      LsHelp(),
      GrepHelp(),
      FindDuplicatesHelp(),
      ReadLinkHelp())
  }
    
  /*
   * AST starts here
   * vvvvvvvvvvvvvvv
   */

  def command: Parser[Command] = (
    source 
  | db
  | tag
  | rm
  | mv 
  | ls 
  | grep
  | find_duplicates 
  | readlink
  | anything ^^ { case _ => OzerError(CommandHelp()) }
  )

  def source: Parser[Command] = (
    arg("source") ~> arg("ls") ~> anything             ^^ { case _ => OzerError(SourceHelp.listHelp) }
  | arg("source") ~> arg("ls")                         ^^ { case _ => Source.List                    }
  | arg("source") ~> (arg("add") ~> rep1(arg(string))) ^^ { case files => Source.Add(files)          }
  | arg("source") ~> arg("add")                        ^^ { case _ => OzerError(SourceHelp.addHelp)  } 
  | arg("source") ~> (arg("rm") ~> rep1(arg(string)))  ^^ { case files => Source.Rm(files)           }
  | arg("source") ~> arg("rm")                         ^^ { case _ => OzerError(SourceHelp.rmHelp)   }
  | arg("source") ~> anything                          ^^ { case _ => OzerError(SourceHelp())        }
  | arg("source")                                      ^^ { case _ => OzerError(SourceHelp())        }
  ) 
  
  def db: Parser[Command] = (
    arg("db") ~> arg("create") ~> arg(string) ~> anything ^^ { case _ => OzerError(DbHelp.createHelp) }
  | arg("db") ~> arg("create") ~> arg(string)             ^^ { case dir => Db.Create(dir)             }
  | arg("db") ~> arg("create")                            ^^ { case _ => OzerError(DbHelp.createHelp) }
  | arg("db") ~> arg("update") ~> anything                ^^ { case _ => OzerError(DbHelp.updateHelp) }
  | arg("db") ~> arg("update")                            ^^ { case _ => Db.Update                    }
  | arg("db") ~> arg("status") ~> anything                ^^ { case _ => OzerError(DbHelp.statusHelp) }
  | arg("db") ~> arg("status")                            ^^ { case _ => Db.Status                    }
  | arg("db") ~> anything                                 ^^ { case _ => OzerError(DbHelp())          }
  | arg("db")                                             ^^ { case _ => OzerError(DbHelp())          }
  )

  def tag: Parser[Command] = (
    tag_auto
  | tag_add
  | tag_rm
  | tag_repair
  | arg("tag") ~ anything ^^ { case _ => OzerError(TagHelp()) }
  )

  def tag_auto: Parser[Command] = (
    arg("tag") ~> arg("auto") ~> anything ^^ { case _ => OzerError(TagHelp.autoHelp) }
  | arg("tag") ~> arg("auto")             ^^ { case _ => Tag.Auto                    }
  )

  def tag_add: Parser[Command] = (
    arg("tag") ~> arg("add") ~> arg(string) ~> arg(string) ~> arg("--") ~> arg(string) ~> anything ^^ { case _ => OzerError(TagHelp.addHelp)                                  }
  | arg("tag") ~> arg("add") ~> (arg(string) ~ arg(string) ~ (arg("--") ~> arg(string)))           ^^ { case key ~ value ~ fileName => Tag.AddTagToFile(key, value, fileName) }
  | arg("tag") ~> arg("add") ~> arg(string) ~> arg(string) ~> arg(string) ~> anything              ^^ { case _ => OzerError(TagHelp.addHelp)                                  }
  | arg("tag") ~> arg("add") ~> (arg(string) ~ arg(string) ~  arg(string))                         ^^ { case key ~ value ~ title => Tag.AddTagToMovie(key, value, title)      }
  | arg("tag") ~> arg("add") ~> anything                                                           ^^ { case _ => OzerError(TagHelp.addHelp)                                  }
  | arg("tag") ~> arg("add")                                                                       ^^ { case _ => OzerError(TagHelp.addHelp)                                  }
  )

  def tag_rm: Parser[Command] = (
    arg("tag") ~> arg("rm") ~> arg(string) ~> arg(string) ~> arg("--") ~> arg(string) ~> anything ^^ { case _ => OzerError(TagHelp.rmHelp)                                    }
  | arg("tag") ~> arg("rm") ~> (arg(string) ~ arg(string) ~ (arg("--") ~> arg(string)))           ^^ { case key ~ value ~ fileName => Tag.RmTagFromFile(key, value, fileName) }
  | arg("tag") ~> arg("rm") ~> (arg(string) ~ (arg("--") ~> arg(string)))                         ^^ { case key ~ fileName => Tag.RmAllTagsFromFile(key, fileName)            }
  | arg("tag") ~> arg("rm") ~> arg(string) ~> arg(string) ~> arg(string) ~> anything              ^^ { case _ => OzerError(TagHelp.rmHelp)                                    }
  | arg("tag") ~> arg("rm") ~> (arg(string) ~ arg(string) ~ arg(string))                          ^^ { case key ~ value ~ title => Tag.RmTagFromMovie(key, value, title)      }
  | arg("tag") ~> arg("rm") ~> arg(string) ~ arg(string)                                          ^^ { case key ~ title => Tag.RmAllTagsFromMovie(key, title)                 }
  | arg("tag") ~> arg("rm") ~ anything                                                            ^^ { case _ => OzerError(TagHelp.rmHelp)                                    }
  | arg("tag") ~> arg("rm")                                                                       ^^ { case _ => OzerError(TagHelp.rmHelp)                                    }
  )

  def tag_repair: Parser[Command] = (
    arg("tag") ~> arg("repair") ~> arg(string) ~> arg(string) ~> anything ^^ { case _ => OzerError(TagHelp.repairHelp)              }
  | arg("tag") ~> arg("repair") ~> arg(string) ~  arg(string)             ^^ { case key ~ title => Tag.RepairTagInMovie(key, title) }
  | arg("tag") ~> arg("repair") ~> arg(string)                            ^^ { case title       => Tag.RepairAllTagsInMovie(title)  }
  | arg("tag") ~> arg("repair")                                           ^^ { case _ => OzerError(TagHelp.repairHelp)              }
  )

  def rm: Parser[Command] = (
    rm_file
  | rm_movie
  )

  def rm_file: Parser[Command] = (
    arg("rm-file") ~> rep1(arg(string)) ^^ { case files => Rm.Files(files)     }
  | arg("rm-file")                      ^^ { case _ => OzerError(RmFileHelp()) }
  )

  def rm_movie: Parser[Command] = (
    arg("rm-movie") ~> arg("-tag") ~> arg(string) ~> arg(string) ~> anything ^^ { case _ => OzerError(RmMovieHelp())                 }
  | arg("rm-movie") ~> arg("-tag") ~> (arg(string) ~ arg(string))            ^^ { case key ~ value => Rm.MovieByKeyValue(key, value) }
  | arg("rm-movie") ~> rep1(arg(string))                                     ^^ { case titles => Rm.Movies(titles)                   }
  | arg("rm-movie")                                                          ^^ { case _ => OzerError(RmMovieHelp())                 }
  )

  def mv: Parser[Command] = (
    mv_file
  | mv_movie
  | mv_all
  )

  def mv_file: Parser[Command] = (
    arg("mv-file") ~> arg(string) ~> arg(string) ~> anything ^^ { case _ => OzerError(MoveFileHelp())                          }
  | arg("mv-file") ~> arg(string) ~ arg(string)              ^^ { case source ~ destination => Move.File(source, destination) }
  | arg("mv-file") ~> anything                               ^^ { case _ => OzerError(MoveFileHelp())                          }
  | arg("mv-file")                                           ^^ { case _ => OzerError(MoveFileHelp())                          }
  )

  def mv_movie: Parser[Command] = (
    arg("mv-movie") ~> arg(string) ~> arg(string) ~> anything ^^ { case _ => OzerError(MoveMovieHelp())                     }
  | arg("mv-movie") ~> arg(string) ~ arg(string)              ^^ { case title ~ directory => Move.Movie(title, directory) }
  | arg("mv-movie") ~> anything                               ^^ { case _ => OzerError(MoveMovieHelp())                     }
  | arg("mv-movie")                                           ^^ { case _ => OzerError(MoveMovieHelp())                     }
  )

  def mv_all: Parser[Command] = (
    arg("mv-all") ~> arg(string) ~> anything ^^ { case _ => OzerError(MoveAllHelp())    }
  | arg("mv-all") ~> arg(string)             ^^ { case directory => Move.All(directory) } 
  | arg("mv-all")                            ^^ { case _ => OzerError(MoveAllHelp())    }
  )

  def ls: Parser[Command] = (
    arg("ls") ~ arg("untagged") ~ anything                          ^^ { case _ => OzerError(LsHelp.untaggedHelp)    }
  | arg("ls") ~ arg("untagged")                                     ^^ { case _ => Ls.Untagged                       }
  | arg("ls") ~ arg("cathegories") ~ anything                       ^^ { case _ => OzerError(LsHelp.cathegoriesHelp) }
  | arg("ls") ~ arg("cathegories")                                  ^^ { case _ => Ls.Cathegories                    }
  | arg("ls") ~> arg("tag") ~> arg("--") ~> arg(string) ~> anything ^^ { case _ => OzerError(LsHelp.tagHelp)         }
  | arg("ls") ~> arg("tag") ~> (arg("--") ~> arg(string))           ^^ { case fileName => Ls.TagsOfFile(fileName)    }
  | arg("ls") ~> arg("tag") ~> arg(string) ~> anything              ^^ { case _ => OzerError(LsHelp.tagHelp)         }
  | arg("ls") ~> arg("tag") ~> arg(string)                          ^^ { case title => Ls.TagsOfMovie(title)         }
  | arg("ls") ~> anything                                           ^^ { case _ => OzerError(LsHelp())               }
  | arg("ls")                                                       ^^ { case _ => Ls.Everything                     }
  )

  def readlink: Parser[Command] = (
    arg("readlink") ~> arg(string) ~> anything ^^ { case _ => OzerError(ReadLinkHelp()) }
  | arg("readlink") ~> arg(string)             ^^ { case file => ReadLink.File(file)    }
  )

  def grep: Parser[Command] = (
    arg("grep") ~> arg(string) ~ arg(string) ~ arg(string) ~ anything ^^ { case _ => OzerError(GrepHelp())                 }
  | arg("grep") ~> arg(string) ~ arg(string) ~ arg(string)            ^^ { case cat ~ tag ~ movie => Grep(cat, tag, movie) }
  | arg("grep") ~> anything                                           ^^ { case _ => OzerError(GrepHelp())                 }
  | arg("grep")                                                       ^^ { case _ => OzerError(GrepHelp())                 }
  )


  def find_duplicates: Parser[Command] = (
    arg("find-duplicates") ~> anything ^^ { case _ => OzerError(FindDuplicatesHelp()) }
  | arg("find-duplicates")             ^^ { case _ => FindDuplicates                  }
  )

  // helpers

  def arg[T](parser: Parser[T]): Parser[T] = separator ~> parser <~ separator
  def anything = rep1(arg(string))
  
  def string = ("[^" + separator + "]*").r
  /*
   * ^^^^^^^^^^^^^^^
   */

  def parseArguments(arguments: Seq[String]): Command = {
    debug("arguments == " + arguments)
    val string = arguments.map { a => separator + a + separator } mkString ""
    parseAll(command, string) match {
      case Success(command, _) => command
      case x => ozer.parsing.Error(x.toString)
    }
  }
  
  def debug(string: String) {
    if (DEBUG) println(string)
  }
  
  val DEBUG = false
}

object Parse extends Parser {
  def apply(arguments: Seq[String]): Command = 
    if (arguments.isEmpty) OzerError(CommandHelp())
    else                   parseArguments(arguments)
}
