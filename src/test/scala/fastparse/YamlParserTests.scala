package fastparse.yaml
import fastparse.all.Parsed

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import org.scalatest.Assertions._

/*
trait ParserTest { this: FunSuite =>
import fastparse.all._
  val parser:Parser[String]

  def t(text:String,expected:String) = 
    test('"'+text.replace("\\n","\\\\n")+'"') {
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
/*
trait QuotedTest { this: FunSuite =>
  def testsFor(cases:(String,String)*) = for(t <- cases) testQuoted(t._1, t._2)
  def testQuoted(text:String, expected:String) = {
    val textEscd = text.replaceAll("\\n","\\\\n")+'"'
    test("BlockKey: "+ textEscd) {
      new YamlParser(0,BlockKey).quoted.parse(text) match {
        case Parsed.Success(x, _) =>
          if(text.contains("\n")) fail()
          else assert(x == expected)
        case failure @ Parsed.Failure(_,_,_) =>
          if(!text.contains("\n")) fail(failure.msg)
      }
    }

    test("FlowOut: "+ textEscd) {
      new YamlParser(0,FlowOut).quoted.parse(text) match {
        case Parsed.Success(x, _) => assert(x == expected)
        case failure @ Parsed.Failure(_,_,_) => fail(failure.msg)
      }
    }
  }
}
/* */
class SingleQuotedScalarTests extends FunSuite with QuotedTest {
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

class DoubleQuotedScalarTests extends FunSuite with QuotedTest {
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
*/

class BlockScalarTests extends FunSuite {
  val parser = new YamlParser(1, FlowOut).block_scalar

  for(t <- List(/*
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
*/
    ">\n   a\n\n    \n   b" -> "a\n \nb"
    //,"|\n  \u271d" -> "\u271d"
  )) {
    val text = t._1
    val expected = t._2
    test('"'+text.replaceAll("\\n","\\\\n")+"\" => \""+expected.replaceAll("\\n","\\\\n")+'"') {
      parser.parse(text) match {
        case Parsed.Success(x, _) => assertResult(expected)(x)
        case failure @ Parsed.Failure(_,_,_) => fail(failure.msg)
      }
    }
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
