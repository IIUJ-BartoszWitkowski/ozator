package ozer

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import ozer.parsing._

@RunWith(classOf[JUnitRunner])
class InterpretSuite extends FunSuite {
  trait MockSourceHandler extends SourceHandler {
    var removed = List.empty[String]
    var added = List.empty[String]

    override def add(files: List[String]) = {
      added ++= files
    }
    override def remove(files: List[String]) = {
      removed ++=  files
    }
    
    def ==(that: MockSourceHandler) = {
      (this.removed == that.removed &&
       this.added   == that.added)
    }
  }

  object MockSourceHandler {
    def apply(sources: List[String] = List.empty, 
       added: List[String] = List.empty,
       removed: List[String] = List.empty) = {
    
      val toAdd = added
      val toRemove = removed
      new MockSourceHandler {
        added ++= toAdd
        removed ++= toRemove
        def list = sources
      }
    }

    def empty = apply(List.empty)
  }

  class MockScreenHandler extends ScreenHandler {
    private var msg = List.empty[String]

    def messages = msg

    def println(message: String) = {
      msg :+= message
    }

    def ==(that: MockScreenHandler) = {
      this.msg = that.msg
    }
  }

  object MockScreenHandler {
    def empty: MockScreenHandler = new MockScreenHandler
  }

  test("source list") {
    def check(given: List[String]) = {
      val mockSourceHandler = MockSourceHandler(given)
      val mockScreenHandler = MockScreenHandler.empty
      
      val interpret = new Interpret {
        override val sourceHandler = mockSourceHandler
        override val screenHandler = mockScreenHandler
      }

      interpret(Source.List)
      
      assert(mockScreenHandler.messages == given)
    }

    check(List("dir1", "dir2"))
    check(List.empty[String])
  }

  test("source add") {
    def check(given: List[String]) = {
      val mockSourceHandler = MockSourceHandler.empty
      val mockScreenHandler = MockScreenHandler.empty
      
      val interpret = new Interpret {
        override val sourceHandler = mockSourceHandler
        override val screenHandler = mockScreenHandler
      }

      interpret(Source.Add(given))

      assert(mockSourceHandler == MockSourceHandler(added = given))
    }

    check(List("dir1", "dir2"))
    check(List.empty[String])
  }

  test("source remove") {
    def check(given: List[String]) = {
      val mockSourceHandler = MockSourceHandler.empty
      val mockScreenHandler = MockScreenHandler.empty
      
      val interpret = new Interpret {
        override val sourceHandler = mockSourceHandler
        override val screenHandler = mockScreenHandler
      }

      interpret(Source.Rm(given))

      assert(mockSourceHandler == MockSourceHandler(removed = given))
    }

    check(List("dir1", "dir2"))
    check(List.empty[String])
  }
}
