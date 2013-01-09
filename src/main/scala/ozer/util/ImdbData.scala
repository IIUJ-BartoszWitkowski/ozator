package ozer.util

import java.io.File
import scala.io.Source
import scala.collection.mutable.OpenHashMap
import scala.collection.mutable.MutableList
import scala.collection.mutable.Buffer
import ozer.data._

object ImdbData {
  def readMovies(path: String): OpenHashMap[String, MutableList[Movie]] = {
    val initialSize = 300000
    val map = new OpenHashMap[String, MutableList[Movie]](initialSize)

    def add(key: String, value: Movie) {
      if (map.isDefinedAt(key)) {
        val list = map(key)
        list += value
      } else {
        map += (key -> MutableList(value))
      }
    }

    for (line <- Source.fromFile(path).getLines()) {
      val lineTokens = line.split("\t").filter(!_.isEmpty)

      assert(lineTokens.size == 2, { 
        "Wrong number of tokens in line `" + line + "`"
      })

      val year = lineTokens(1)
      val title = lineTokens(0)

      val movie = Movie(title, year)

      val words = title.split("[^a-zA-Z0-9]").filter(!_.isEmpty).distinct
      for (t <- words) {
        add(t.toLowerCase, movie)
      }

      val strangeChars = title.replaceAll("[a-zA-Z0-9\\(\\)\\s\\[\\]]", "").distinct
      for (c <- strangeChars) {
        add(c.toString, movie)
      }
    }
    map
  }

  def printToFile(file: File)(f: java.io.PrintWriter => Unit) {
    val printWriter = new java.io.PrintWriter(new java.io.FileWriter(file, true))
    try { f(printWriter) } finally { printWriter.close() }
  }

  def getFile(rootDir: String, hash: String, createDirs: Boolean = false) = {
    val dirPath = hash.slice(0, 2) 
    val fileName = hash.drop(2)
    val dir = new File(rootDir, dirPath) 
    if (createDirs) {
      dir.mkdirs
      assert(dir.exists)
    }
    new File(dir, fileName)
  }

  def hash(string: String) = {
    val sha1 = java.security.MessageDigest.getInstance("SHA-1")
    val bytes = sha1.digest(string.getBytes)
    bytes.map("%02X" format _).mkString.toLowerCase
  }

  def writeCache(dir: String, map: OpenHashMap[String, MutableList[Movie]]) {
    map foreach { case (key, movies) => 
      val nameHash = hash(key)
      val file = getFile(dir, nameHash, createDirs = true)

      printToFile(file) { p =>
        p.print(key + "\t")
        movies.foreach { m =>
          p.print(m.title + "\t")
          p.print(m.year + "\t")
        }
        p.println
      }
    }
  }

  def words(string: String): Seq[String] = string.split("[^a-zA-Z0-9]") filter (!_.isEmpty)

  def strangeChars(string: String): Seq[String] = 
    string.replaceAll("[a-zA-Z0-9\\(\\)\\s]", "").toCharArray map (_.toString)


  def isAlpha(c: Char) = c.isLower || c.isUpper
  def isAlphaNum(c: Char) = isAlpha(c) || c.isDigit

  def isNumeric(s: String) = s.forall(_.isDigit)

  /**
   * consider two strings:
   * abc
   * bcab
   *
   * <pre>
   * 1 pass:
   *
   * a != b
   * b == b
   * c != b
   *   =>
   * 0 0 0 0
   * 1 0 0 0
   * 0 0 0 0
   *
   * 2 pass:
   * 
   * a != c
   * b != c
   * c == c
   *
   * 0 0 0 0
   * 1 0 0 0
   * 0 2 0 0
   *
   * 3 pass:
   *
   * a == a
   * b != a
   * c != a
   *
   * 0 0 1 0
   * 1 0 0 0
   * 0 2 0 0
   *
   * 4 pass:
   *
   * a != b
   * b == b
   * c != b
   *
   * 0 0 1 0
   * 1 0 0 2
   * 0 2 0 0
   * </pre>
   *
   */
  def longestCommonSubstring(left: Seq[String], right: Seq[String]): Int = {
    val table = new Array[Array[Int]](left.length)
    for (i <- 0 to left.length - 1) {
      table(i) = new Array[Int](right.length)
    }

    var substringLength = 0

    val ret = MutableList.empty[Seq[String]]

    for (i <- 0 until left.length; j <- 0 until right.length) {
      if (left(i) == right(j)) {
        if (i == 0 || j == 0) table(i)(j) = 1
        else                  table(i)(j) = table(i - 1)(j -1) + 1
        
        if (table(i)(j) > substringLength) {
          substringLength = table(i)(j)
          ret.clear
          ret += (left.slice(i - substringLength, i + 1))
        } else if (table(i)(j) == substringLength) {
          ret += left.slice(i - substringLength, i + 1)
        }
      } else {
        table(i)(j) = 0
      }
    }

    substringLength
  }

  def getPropositions(name: String, cacheRoot: String): Seq[(Movie, Int)] = {
    val hashMap = new OpenHashMap[Movie, Int](200)
    val tokens = (words(name) ++ strangeChars(name)) map (_.toLowerCase) distinct

    def add(movie: Movie, weight: Int) {
      if (hashMap.isDefinedAt(movie)) {
        val count = hashMap(movie)         
        hashMap(movie) += weight
      } else {
        hashMap(movie) = weight
      }
    }

    tokens foreach { token => 
      val file = getFile(cacheRoot, hash(token))
      if (file.exists) {
        val lines = Source.fromFile(file).getLines
        lines.foreach { line =>
          val lineTokens = line.split("\t").filter(!_.isEmpty)
          val keyword = lineTokens(0)
        
          if (token == keyword) {

            val list = lineTokens.drop(1).grouped(2)
            val weight = {
              if (keyword.length > 1) 4
              else {
                val c = keyword.charAt(0)
                if (isAlphaNum(c)) 2 else 1
              }
            }

            val movies = for (titleYear <- list) {
              val movie = Movie(titleYear(0), titleYear(1))
              add(movie, weight)
            }
          }
        }
      }
    }

    val withLongestCommonSubstring = hashMap.toSeq map { case (movie, score) =>
      val fileNameWords = words(name)
      val movieWords = words(movie.title)
      val common = longestCommonSubstring(fileNameWords, movieWords)

      (movie, score + common)
    }

    withLongestCommonSubstring.sortBy(_._2).reverse
  }


  def getBestPropositions(name: String, cache: String): Seq[Movie] = {
    val nBest = 5
    val props = getPropositions(name, cache)
    if (props.isEmpty) {
      return Seq.empty[Movie]
    }

    val best = MutableList.empty[(Movie, Int)]
    var currentMax = props(0)._2
    var take = true
    var taken = 0
    props.foreach { p =>
      if (take) {
        val score = p._2
        if (taken > nBest && score < currentMax) {
          take = false 
        } else {
          best += p
          taken += 1
          currentMax = score
        }
      }
    }
    val sorted = best.sortWith((l, r) => 
      if (l._2 > r._2) l._2 > r._2 
      else l._1.title.length < r._1.title.length)
    sorted.take(5).map{case (m, p) => m}toSeq
  }

  def getGenres(file: String, name: String): Seq[String] = {
    val genres = Buffer.empty[String]

    val lines = Source.fromFile(file).getLines

    var startSearching = false
    var lastLine = ""
    var keepReading = true
    var found = false

    lines takeWhile(x => keepReading) foreach { line =>
      if (!startSearching) {
        if (lastLine.contains("8: THE GENRES LIST") && line.contains("======")) {
          startSearching = true
        } else {
          lastLine = line
        }
      } else {
        if (!line.isEmpty) {
          val lineTokens = line.split("\t").filter(!_.isEmpty)

          assert(lineTokens.size == 2, { 
            "Wrong number of tokens in line `" + line + "`"
          })

          val title = lineTokens(0)
          if (title.trim == name) {
            found = true
            val genre = lineTokens(1)
            genres += genre
          } else if (found) {
            keepReading = false
          }
        }      
      }
    }
    genres
  }

  def getRating(file: String, name: String): Option[String] = {
    val lines = Source.fromFile(file).getLines

    var startSearching = false

    lines foreach { line =>
      if (!startSearching) {
        if (line.contains("MOVIE RATINGS REPORT")) {
          startSearching = true
        } 
      } else {
        if (line.isEmpty) {
          // do nothing
        } else if (line.contains("-------------")) {
          return None
        } else {
          val rating = line.slice(27, 30)
          val title = line.drop(32)

          if (title.trim == name) {
            return Some(rating)
          } 
        }      
      }
    }
    None
  }

  def getDirectors(file: String, name: String): Seq[String] = {
    val directors = Buffer.empty[String]

    val lines = Source.fromFile(file).getLines

    var startSearching = false
    var lastLine = ""
    var director = ""

    lines foreach { line =>
      if (!startSearching) {
        if (lastLine.contains("THE DIRECTORS LIST") && line.contains("======")) {
          startSearching = true
        } else {
          lastLine = line
        }
      } else {
        if (line.isEmpty) {
          // do nothing
        } else if (line.contains("----------")) {
          return directors;
        } else {
          val lineTokens = line.split("\t").filter(!_.isEmpty)

          if (lineTokens.size == 2) {
            director = lineTokens(0)
          }
          val title = lineTokens(lineTokens.size - 1)

          if (title.trim == name) {
            directors += director
          } 
        }      
      }
    }
    directors
  }

  def getActors(file: String, names: Seq[String]): Map[String, Seq[Actor]] = {
    val actors = new OpenHashMap[String, Buffer[Actor]] 

    def add(name: String, actor: Actor) = {
      if (actors.isDefinedAt(name)) {
        val buff = actors(name)
        buff += actor
      } else {
        actors(name) = Buffer(actor)
      }
    }

    val lines = Source.fromFile(file).getLines

    var startSearching = false
    var lastLine = ""
    var actor = ""

    def isListStart(line: String) = {
      line.contains("THE ACTRESSES LIST") ||
      line.contains("THE ACTORS LIST")
    }

    lines foreach { line =>
      if (!startSearching) {
        if (isListStart(lastLine) && line.contains("======")) {
          startSearching = true
        } else {
          lastLine = line
        }
      } else {
        if (line.isEmpty) {
          // do nothing
        } else if (line.contains("----------")) {
          return actors.toMap
        } else {
          val lineTokens = line.split("\t").filter(!_.isEmpty)

          if (lineTokens.size == 2) {
            actor = lineTokens(0)
          }
          val titleRoleBilling = lineTokens(lineTokens.size - 1).split("  ")

          val title = titleRoleBilling(0)

          val billing = {
            def isBillingToken(str: String) = {
              str.contains("<") && 
              str.contains(">") && 
              isNumeric(str.replaceAll("[<>]", ""))
            }

            if (titleRoleBilling.size < 2) {
              100 
            } else  {
              val lastToken = titleRoleBilling.last
              if (isBillingToken(lastToken)) {
                lastToken.replaceAll("[<>]", "").toInt
              } else {
                100
              }
            }
          }

          names foreach { name => 
            if (title.trim == name) {
              add(name, Actor(actor, billing))
            } 
          }
        } 
      }
    }
    actors.toMap
  }

  def getActorsAndActresses(
      actorsFile: String, 
      actressesFile: String, 
      names: Seq[String]): 
    Map[String, Seq[Actor]] = {

    val hashMap = new OpenHashMap[String, Seq[Actor]]

    val actors    = getActors(actorsFile, names)
    val actresses = getActors(actressesFile, names)
 
    names foreach { name =>
      hashMap(name) = (
        actors.getOrElse(name, Buffer.empty[Actor]) ++ 
        actresses.getOrElse(name, Buffer.empty[Actor])).sortBy(_.billing)
    }

    hashMap.toMap
  }
}

/*

val path = "movies.txt"
val cache = readMovies(path)

val cacheDir = "/home/nekro/.ozer/cache"
writeCache(dir, cache)

val sliceSize = 5
val n = 13
val slice = fileList.slice(n * sliceSize, (n + 1) * sliceSize)

val names = Seq(
  "Donnie Darko (2001)",
  "The Last Samurai (2003)")

for ( name <- slice ) {
  println("name: " + name) 
  println(getBestPropositions(name, dir).mkString("\n"))
  println("--------------------------\n")
}

  println(getPropositions(name, dir).filter{ case (m, i) => i == 12 })

val fileName = "Drive.2011.720p.BDRip.XviD.AC3-ViSiON"
val props = getBestPropositions(fileName, cacheDir)

val name = "Donnie Darko (2001)"

val genresFile = "/home/nekro/.ozer/genres.utf8"
val genres = getGenres(genresFile, name)

val ratingsFile = "/home/nekro/.ozer/ratings.utf8"
val rating = getRating(ratingsFile, name)

val directorsFile = "/home/nekro/.ozer/directors.utf8"
val directors = getDirectors(directorsFile, name)

val actorsFile = "/home/nekro/.ozer/actors.utf8"
val actressesFile = "/home/nekro/.ozer/actresses.utf8"

val actors = getActors(actorsFile, names)

val actors = getActorsAndActresses(actorsFile, actressesFile, names)

	*/
