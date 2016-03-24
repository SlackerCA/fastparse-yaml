package fastparse.yaml
import fastparse.all.{Parser}
import fastparse.core.Parsed //https://github.com/lihaoyi/fastparse/issues/34

import org.scalatest.FunSuite
import org.scalatest.Assertions._
import org.scalatest.{Ignore,DoNotDiscover}

trait ParserTest { this: FunSuite =>
  def testParse(parser:Parser[String])(cases:(String,String)*):Unit = for(t <- cases) testParse(t._1, t._2)(parser)
  //def testParse(cases:(String,String)*)(implicit parser:Parser[String]):Unit = for(t <- cases) testParse(t._1, t._2)
  //def testParse(t:(String,String))(implicit parser:Parser[String]):Unit = testParse(t._1,t._2)
  def testParse(text:String, expected:String)(implicit parser:Parser[String]):Unit = {
    test('"'+text.replaceAll("\\n","↵")+'"') {
      parser.parse(text) match {
        case Parsed.Success(x, _) => assertResult(expected)(x)
        case failure @ Parsed.Failure(_,_,_) => fail(failure.msg)
      }
    }
  }
}
/*
import fastparse.all._
  val parser:Parser[String]

  def t(text:String,expected:String) = 
    test('"'+text.replace("\\n","↵")+'"') {
      parser.parse(text) match {
        case Parsed.Success(x, _) => assert(x == expected)
        case failure @ Parsed.Failure(_,_,_) => fail(failure.msg)
      }
    }
}

class TerminalParserTests extends FunSuite with ParserTest {
  import fastparse.all._
  import YamlParsers._
  private val s_indent = s_space.rep(min=1,max=1)
  val parser = 
    (s_indent.log("s_indent") ~ nb_char.rep(1).!.log()).log("parser")
  //t(" a","a") 
}
*/


trait QuotedTest { this: FunSuite =>
  val parserBlockKey:Parser[String] = new YamlParser(0,BlockKey).quoted
  val parserFlowOut:Parser[String] = new YamlParser(0,FlowOut).quoted
  def quoted(s:String) = s
  def testsFor(cases:(String,String)*) = for(t <- cases) testQuoted(t._1, t._2)
  def testQuoted(plaintext:String, expected:String) = {
    val text = quoted(plaintext)
    val testName = '"' + text.replaceAll("\\n","↵") + '"'
    test("BlockKey "+ testName) {
      parserBlockKey.parse(text) match {
        case Parsed.Success(x, _) =>
          if(text.contains("\n")) fail()
          else assert(x == expected)
        case failure @ Parsed.Failure(_,_,_) =>
          if(!text.contains("\n")) fail(failure.msg)
      }
    }

    test("FlowOut "+ testName) {
      parserFlowOut.parse(text) match {
        case Parsed.Success(x, _) => assert(x == expected)
        case failure @ Parsed.Failure(_,_,_) => fail(failure.msg)
      }
    }
  }
}
//@DoNotDiscover
class SingleQuotedScalarTests extends FunSuite with QuotedTest {
  override def quoted(s:String) = '\'' + s + '\''
  testsFor (
    "" -> "",
    " " -> " ",
    "abc" -> "abc",
    "a b\tc" -> "a b\tc",
    "a\nb" -> "a b",
    "a\n\nb" -> "a\nb",
    "  a" -> "  a",
    "a  " -> "a  ",
    "  a  \n  b  " -> "  a b  ",
    "  \na\nb\n  " -> " a b ",
    "that''s" -> "that's"
  )
}
//@DoNotDiscover
class DoubleQuotedScalarTests extends FunSuite with QuotedTest {
  override def quoted(s:String) = '"' + s + '"'
  testsFor (
    "" -> "",
    " " -> " ",
    "abc" -> "abc",
    "a b\tc" -> "a b\tc",
    "a\nb" -> "a b",
    "a\n\nb" -> "a\nb",
    "  a" -> "  a",
    "a  " -> "a  ",
    "  a  \n  b  " -> "  a b  ",
    "  \na\nb\n  " -> " a b ",
    "a\\nb" -> "a\nb"
  )
}
//@DoNotDiscover
class PlainScalarTests extends FunSuite with ParserTest {
  testParse (new YamlParser(0,FlowOut).plain)(
    //"" -> "",
    //" " -> " ",
    "abc" -> "abc",
    "a b\tc" -> "a b\tc",
    "a\nb" -> "a b",
    "a\n\nb" -> "a\nb",
    "a  " -> "a",
    "a  \n  b  " -> "a b",
    "a\nb\n  " -> "a b",
    "?a" -> "?a"
  )
}
//@DoNotDiscover
class BlockScalarTests extends FunSuite with ParserTest {
  testParse(new YamlParser(1, FlowOut).block_scalar)(
    "|\n  a" -> "a",
    "| #f\n  a" -> "a",
    "|\n  a\n" -> "a\n",
    "|\n   a\n    b" -> "a\n b",
    "|\n   a\n\n   b" -> "a\nb",
    "|1\n a\n" -> "a\n",
    "|-\n  a\n" -> "a",
    "|+\n  a\n\n" -> "a\n\n",
    "|1-\n a\n" -> "a",
    "|-1\n a\n" -> "a",
    ">\n   a\n   b" -> "a b",
    ">\n   a\n\n\n   b" -> "a\n\nb",
    ">\n   a\n\n   \n   b" -> "a\n\nb",
    ">\n   a\n\n    \n   b" -> "a\n \nb",
    "|\n  \u271d" -> "\u271d"
  )
}

//@DoNotDiscover
class YamlApiTests extends FunSuite {
  import Yaml._
  test("scalar") {
    assertResult("foo"){doc(scalar).parse("foo")}
  }
  test("map") {
    assertResult(Map("foo"->"bar")){
      doc("foo"->scalar).parse("foo : bar")
    }
  }
  test("typical map doc") {
    assertResult(Map("foo"->"bar","bar"->"foo")){
      doc("foo"->scalar, "bar"->scalar)
        .parse("foo : bar\nbar : foo")}
  }
  test("nested map") {
    assertResult(Map("foo"->Map("bar"->"blitz"))){
      doc(
        "foo"-> map(
          "bar"->scalar,
          "baz"->scalar)
      )
        .parse("foo : \n bar : blitz\n")}
  }

}
//TODO: tests for unsupported features like aliases

/* TODO: tag not run by default
class FastParseTests extends FunSuite {
  import fastparse.all._
  val s_indent = P(" ").rep(min=2,max=2)
  test("s_indent"){ s_indent.parse("   a") match {
    case Parsed.Success(x, _) => ()
    case failure @ Parsed.Failure(_,_,_) => fail(failure.msg)
  }
    }
}
*/
