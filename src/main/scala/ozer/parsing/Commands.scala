package ozer.parsing

sealed abstract trait Command
case class Error(message: String) extends Command

final object Source {
  case object List extends Command
  case class Add(files: List[String]) extends Command
  case class Rm(files: List[String])  extends Command
}

final object Db {
  case class Create(directory: String) extends Command
  case object Update extends Command
  case object Status extends Command
}

final object Tag {
  case object Auto extends Command 
  case class AddTagToFile(key: String, value: String, fileName: String)  extends Command 
  case class AddTagToMovie(key: String, value: String, title: String)  extends Command 
  case class RmAllTagsFromFile(key: String, fileName: String)  extends Command 
  case class RmAllTagsFromMovie(key: String, title: String)  extends Command 
  case class RmTagFromFile(key: String, value: String, fileName: String)  extends Command 
  case class RmTagFromMovie(key: String, value: String, title: String)  extends Command 
  case class RepairAllTagsInMovie(title: String) extends Command 
  case class RepairTagInMovie(key: String, title: String) extends Command 
}

final object Rm {
  case class Files(files: List[String]) extends Command
  case class Movies(titles: List[String]) extends Command
  case class MovieByKeyValue(key: String, value: String) extends Command
}

final object Ls {
  case object Untagged extends Command
  case object Everything extends Command
  case object Cathegories extends Command
  case class TagsOfMovie(title: String) extends Command
  case class TagsOfFile(fileName: String) extends Command
}

final object Move {
  case class File(source: String, destination: String) extends Command
  case class Movie(title: String, directory: String) extends Command
  case class All(directory: String) extends Command
}

case class Grep(cathegoryName: String, tagPattern: String, moviePattern: String) extends Command

case object FindDuplicates extends Command

object ReadLink {
  case class File(fileName: String) extends Command
  case class Movie(name: String) extends Command
}
  
