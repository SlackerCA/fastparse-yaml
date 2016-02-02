package fastparse.yaml

import fastparse.all._

object Main extends App {
//  val char(c:Char):Boolean = List(('\u0009','\u0085'),('\u0020' to '\u007E'),('\u00A0' to '\uD7FF'),('\uE000' to '\uFFFD')).forAll(_.contains(c))
  //('\u0021' to '\u007E').diff("-?:,[]{}#&*!|>'\"%@`")//.foreach(print)

  //(('\u0020' to '\u007E') :+ '\u0009')//.foreach(print)
  //  .map((c:Char) => "%s\t\\u%04X".format(c, c.toInt))
  //  .foreach(println)
  //val "-?:,[]{}#&*!|>'\"%@`".sortWith(_ < _) //.foreach(print)
  //val s = "-?:,[]{}#&*!|>'\"%@`"
  //('\u0020' to '\u007E')
  //.filter(!s.contains(_))
  //  .map(c => "%s\t\\u%04X".format(c, c.toInt))
  //  .foreach(println)

//   println(Raml(
// """"title": Title
// foo: Foo
// description: Description"""))
//println(Raml("{ a:b, c:d }"))
  val kv = P(CharIn('a' to 'z').rep.! ~ ": " ~ CharIn('a' to 'z').rep.!)
  val p = P( "{" ~ kv ~ "}" ).parse("{foo: bar}")
  println(p match {
    case Parsed.Success((x,y), _) => "Success:" + x + ": " + y
    case Parsed.Success(x, _) => "Success:" + x
    case fail @ Parsed.Failure(_,_,_) => "Fail:" + fail.msg
  })

}

class RamlParser extends YamlParser {

  def apply(in: String): String = {
    // map(p:Parser) = map_entry.map(p).seq.!
    //val p = map(("title",scalar title_),
    // 
    //            ("description", scalar description_),
    //            ("resource", map( (scalar, resource))
    //
    val p = P(c_flow_mapping.! ~ "".!)
    p.parse(in) match {
      case Parsed.Success(x, _) => "Success:" + x._1 + " : " + x._2
      case fail @ Parsed.Failure(_,_,_) => "Fail:" + fail.msg
    }
  }

  //def resource = map( ( P(""), )
  // map: if key parser succeeds then value parser
  // seq: use (x|y|z).rep
  // scalar: value parses
// 


/*  def apply(in: String): Raml {
  def scalar(key:String) = P(key)

  resources: List[Resource]
  types: List[Type]

  title
  | description
  | resources: List[Resource],
  | types: List[Type]

  }
key X -> (scalar fn)
key S -> (seq fn)
key M -> (match)

match


  def raml: Parser[Raml] = 
      str ("title")
      | seq("resources")
      | stringLiteral
      | floatingPointNumber ^^ (_.toDouble) 
      | "null"  ^^ (x => null) 
      | "true"  ^^ (x => true) 
      | "false" ^^ (x => false)
    var 
    nodes match {
      case Scalar("description",v) => 
    }
  */
}

class Resource
class Type

class Raml (
  title: String,
  description: String,
  resources: List[Resource],
  types: List[Type]
)

