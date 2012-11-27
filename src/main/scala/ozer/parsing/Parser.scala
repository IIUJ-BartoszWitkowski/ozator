package ozer.parsing

import ozer.parsing.{Error => OzerError}
import scala.util.parsing.combinator.RegexParsers

class Parser extends RegexParsers {
  val separator = '|'

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

  object CommandHelp extends Pretty {
    def apply() = list(
      SourceHelp(),
      DbHelp())
  }
    
  /*
   * AST starts here
   * vvvvvvvvvvvvvvv
   */

  def command: Parser[Command] = (
    source 
  | db
  /*| tag
  | rm
  | mv
  | ls
  | grep
  | find_duplicates */
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
  | arg("db")                                             ^^ { case _ => OzerError(DbHelp())          }
  )

  /*
  def tag: Parser[Command] = (
    arg("tag") ~> arg("auto")
  | tag_add
  | tag_rm
  )

  def tag_add: Parser[Command] = (
    arg("tag") ~> arg("add") ~> (arg(string) ~ arg(string) ~ (arg("--") ~> arg(string)))
  | arg("tag") ~> arg("add") ~> (arg(string) ~ arg(string) ~  arg(string))
  )

  def tag_rm: Parser[Command] = (
    arg("tag") ~> arg("rm") ~> arg(string) 
    arg("tag") ~> arg("rm") ~> (arg(string) ~ arg(string)))
    arg("tag") ~> arg("rm") ~> (arg("--") ~> arg(string)))
    arg("tag") ~> arg("rm") ~> (arg(string) ~ (arg("--") ~> arg(string)))
  )

  def tag_repair: Parser[Command] = (
    arg("tag") ~> arg("repair") ~> arg(string)
  | arg("tag") ~> arg("repair") ~> arg(string) ~ arg(string) 
  )

  def rm: Parser[Command] = (
    rm_file
  | rm_movie
  )

  def rm_file = (
    arg("rm-file") ~> rep1(arg(string)) 
  )

  def rm_movie = (
  | arg("rm-movie") ~> title 
  | arg("rm-movie") ~> -("tag") ~> string ~ string 
  )

  def mv: Parser[Command] = (
    mv_file
  | mv_movie
  | mv_all
  )

  def mv_file: Parser[Command] = (
    arg("mv-file") ~> arg(string) ~ arg(string)  
  )

  def mv_movie: Parser[Command] = (
    arg("mv-movie") ~> title ~ directory ^^
  )

  def mv_all: Parser[Command] = (
    arg("mv-all") ~> arg(string) 
  )

  def ls: Parser[Command] = (
    arg("ls") ~ arg("untagged")
  | arg("ls") ~> arg("tag") ~> arg(string)
  | arg("ls") ~> arg("tag") ~> (arg("--") ~> arg(string))
  | arg("ls") ~> -("L") ~ arg(string)
  | arg("ls")
  )

  def grep: Parser[Command] = (
    arg("grep") ~> arg(string) ~ arg(string)
  )

  def find_duplicates: Parser[Command] = (
    arg("find-duplicates")
  )
  */

  // helpers

  def -[T](parser: Parser[T]): Parser[T] = arg("-" ~> parser)
  def any_- : Parser[Any] = arg("-" + string) ~ string*
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
