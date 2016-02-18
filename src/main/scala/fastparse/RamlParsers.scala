package fastparse.yaml

import fastparse.all._

object Main //extends App
{
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
/*
class RamlParser {

  def apply(in: String): String = {
    import YamlParser._
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
 */
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

}

read YAML into structre
Raml( //map
 title: String //scalar
 
)

use generic YAML parser
generate specific parser(s) which are simpler and match on exact trees (serialization)
need to support dynamic parser since RAML uses it's own referencing. flatMap


generics do not work with recursion

push Node to a match

ScalarNode : Parser[ScalarNode]
ListNode : Parser[Seq[Node]].map((Seq[Node])->(ListNode))
 Seq[Node]

MapNode : Parser[MapNode]
 Seq[(Node,Node)] //vs 

MapEntryNode 
 key: Node (which may be 
 value: Node


ScalarNode => 
_ => Unit //ignore

 
as a dev I want to provide a template, which reads like a yaml file.
this needs to allow for dynamic sections, such that the 

//map as a Map
map(Scalar(typeName) => typeName) 

//map where order matters, in that the later parser behaves based on the former
map(
KeyValueNode("special type",Any).flatMap( specialThing )  
// rather then each time generate a new parser, we merly want to accumulate 
// result is new state (accumulated value), such that 
bascially later-parser(earlier-parser(..))

parent needs to treat content as 

first section (
detector-parser 
).flatMap(
second section
dependent-parser
)
which requires partial yaml list/map  processing in the first section
partial list processing 
  while node in (), can not contain Any
  negative lookahead

partial map processing

another way:
no reference checking at parse time. n pass parsing. 


parser-detector(result from last parser-detector)
Success.flatMap(dependent-parser(result from last parser-detector))
dependent-parser(context)

)

List[_]-> ListNode
Map[_] -> MapNode

(x->y | a->b).rep -> MapNode
ListNode(l|m|n) => (l|m|n).rep
ListNode(l ~ m ~ n) => (l ~ m ~ n).rep
MapNode(a->l, b->m, c->n) => (a ~ l | b->m | c->n).rep
 a: ListNode
 b: MapNode 
 c: Scalar


Parser
 SeqNode
 MapNode
 ScalarNode

 Either
 Optional
 

Yaml(
 ListNode // complete Yaml List, implied Either
 MapNode // complete Yaml Mapping, implied Either
 ScalarNode

 SplitNode( // may be a partial list or map
 // stop condition: lookahead, 
 // 
 )
 Node.rep() //  partial list or map
)

non-scalar keys mean we don't know what type a node is untill after it is parsed, 
since we want to often skip unmatched, most of the time we don't know what type a node is untill after it is parsed. 
?If MapNode can check that none of it's keys is a MapNode

Node
for each type
 if a type is missing, use a no-capture parser for that type



how to choose between two map or list if all enteries are optional?

 
val r:Raml
//val raml = (title | description | resources).rep // list
val raml = map(title, description, resources)
val title = entry("title",scalar.!).map(r.title = _)
val description = entry("description",scalar.!).map(r.description_)
val types = entry("types",type.!).map(r.types_)

Yaml.
 map(
 k -> string.map(),
 sk -> list ( a|b|c )
 s2 -> list ( a,b,c )
 ).map(Map[K,V]=>T):T



 

  */
class Resource
class Type

class Raml {
  title: String,
  description: String,
  resources: List[Resource],
  types: List[Type]
}

