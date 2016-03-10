package fastparse.yaml
import fastparse.all.Parsed

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import org.scalatest.Assertions._


trait QuotedTest { this: FunSuite =>
  def testsFor(cases:(String,String)*) = for(t <- cases) testQuoted(t._1, t._2)
  def testQuoted(text:String, expected:String) = {
    test("BlockKey:"+ text) {
      new YamlParser(0,BlockKey).quoted.parse(text) match {
        case Parsed.Success(x, _) =>
          if(text.contains("\n")) fail()
          else assert(x == expected)
        case failure @ Parsed.Failure(_,_,_) =>
          if(!text.contains("\n")) fail(failure.msg)
      }
    }

    test("FlowOut:"+ text) {
      new YamlParser(0,FlowOut).quoted.parse(text) match {
        case Parsed.Success(x, _) => assert(x == expected)
        case failure @ Parsed.Failure(_,_,_) => fail(failure.msg)
      }
    }
  }
}
class SingleQuotedScalars extends FunSuite with QuotedTest {
  testsFor (
    "''" -> "",
    "' '" -> " ",
    "'abc'" -> "abc",
    "'a b\tc'" -> "a b\tc",
    "'a\nb'" -> "a b",
    "'a\n\nb'" -> "a\nb",
    "'  a'" -> "  a",
    "'a  '" -> "a  ",
    "'  a  \n  b  '" -> "  a b  ",
    "'  \na\nb\n  '" -> " a b ",
    "'that''s'" -> "that's"
  )
}

class DoubleQuotedScalars extends FunSuite with QuotedTest {
  def q(s:String) = '"' + s + '"'
  testsFor (
    q("") -> "",
    q(" ") -> " ",
    q("abc") -> "abc",
    q("a b\tc") -> "a b\tc",
    q("a\nb") -> "a b",
    q("a\n\nb") -> "a\nb",
    q("  a") -> "  a",
    q("a  ") -> "a  ",
    q("  a  \n  b  ") -> "  a b  ",
    q("  \na\nb\n  ") -> " a b ",
    q("a\\nb") -> "a\nb"
  )
}
