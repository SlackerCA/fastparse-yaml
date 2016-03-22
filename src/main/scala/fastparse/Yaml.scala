/*

=Design Goals=
- easy to learn, ideal for one time use
 - no special syntax
 - use Scala type names rather than YAML
 - hide/ignore YAML "features"
- simple for the default case
 - order of list and map entries in document does not matter
 - tuple for a short list of fixed size
- mostly used for short one time parse
 - performance is a lower priority


=Maps=
Maps form the backbone as parser only branches on map keys.


map(
"foo" -> string,
"bar" -> mapOf(string->string).map(m:Map[String,String]=>new Foo(m)),
"annotations" -> collect("(.*)".r -> annotation) //Map[String,Annotation]
collect("\\?.*)".r -> optional)
)

).map(m:((String,String),(String,Foo)),(Map[String,Annotation])=> Obj(m._1._2,m._2._2)
.map(match{ case (("foo",foo:String),("bar",bar:Foo)) => Obj(foo,bar) })

=Lists=
listOf(n:Node) => list(n.rep())


=Scalar patterns=
string(filter
string(prefix:String="",suffix:String="")
""
"".r
String => Boolean
Parser

=flatMap=

optional()
either() can not mix element types (simple)
anyof() can mix types (complex)

=======
just produce case classes (list,map,scalar) for second pass parsing
parse() calls match

import Yaml._
document(
"title"->string,
"documentation" -> listOf(map...)
"annotations"-> map(
(string -> map)(match { case (k:String,v:Map) if k.startsWith("/") => (k,v)})
)()

) match {
case m:Map => // val Raml(m.get("title"),m.get("descirption"),m.get("annotations").map(
}

*/

package fastparse.yaml

import fastparse.all._

object Yaml {
  //def apply(pairs:(Element[K], Element[T])*):T = doc(map(pairs))
  def doc[T](root:Element[T]) = new Document[T](root)
  def map[K,T](pairs:(Element[K], Element[T])*):MapElement[K,T] = new MapElement(pairs)
  val scalar:Element[String] = new ScalarElement(s=>true) // AnyScalar
  def scalar(text:String):Element[String] = new ScalarElement(text.== _)
  def scalar(filter:(String => Boolean)):Element[String] = new ScalarElement(filter)
}

class Document[T] (root:Element[T]) {
  val parser = root.block_node(YamlParser(-1,BlockIn))
  // [210]   l-any-document                     ::= l-directive-document | l-explicit-document | l-bare-document
  //  [209]   l-directive-document               ::= l-directive+ l-explicit-document
  //  [208]   l-explicit-document                ::= c-directives-end ( l-bare-document | ( e-node s-l-comments ) )
  //   [203]   c-directives-end                   ::= “-” “-” “-”
  //  [207]   l-bare-document                    ::= s-l+block-node(-1,block-in) /* Excluding c-forbidden content */
  //   [206]   c-forbidden                        ::= /* Start of line */ ( c-directives-end | c-document-end ) ( b-char | s-white | /* End of file */ )
  def parse(text:String):T = {
    parser.parse(text) match {
      case Parsed.Success(x, _) => x
      case failure @ Parsed.Failure(_,_,_) => throw new Exception(failure.msg)
    }
  }
}

abstract class Element[T] {
  import YamlParsers._
  type Y = YamlParser

  /** Creates a parser that tries each given parser in turn, and fails only if all fail. */
  protected def either[T](p: Seq[Parser[T]]): Parser[T] = fastparse.parsers.Combinators.Either(p:_*)

  def block_node(y:Y):Parser[T] = Fail
  // [185]   s-l+block-indented(n,c)            ::= ( s-indent(m) ( ns-l-compact-sequence(n+1+m) | ns-l-compact-mapping(n+1+m) ) ) | s-l+block-node(n,c) | ( e-node s-l-comments )
  //  [186]   ns-l-compact-sequence(n)           ::= c-l-block-seq-entry(n) ( s-indent(n) c-l-block-seq-entry(n) )*
  //  [195]   ns-l-compact-mapping(n)            ::= ns-l-block-map-entry(n) ( s-indent(n) ns-l-block-map-entry(n) )*
  def block_indented(y:Y):Parser[T] = Fail
  def e_node:Parser[T] = Fail
  def yaml_content(y:Y):Parser[T] = Fail
  def json_content(y:Y):Parser[T] = Fail

  // [159]   ns-flow-yaml-node(n,c)             ::= c-ns-alias-node | ns-flow-yaml-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-yaml-content(n,c) ) | e-scalar ) )
  def yaml_node(y:Y):Parser[T] = Fail
    //y.alias_node ~/ Fail |
    //TODO: in scalar flow_yaml_content | ( y.properties ~ P( ( y.separate ~ flow_yaml_content ) | e_scalar ) )


  // [161]   ns-flow-node(n,c)                  ::= c-ns-alias-node | ns-flow-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-content(n,c) ) | e-scalar ) )
  //  [158]   ns-flow-content(n,c)               ::= ns-flow-yaml-content(n,c) | c-flow-json-content(n,c)
  def flow_node(y:Y):Parser[T] = Fail
    //val flow_content = yaml_content(y) | P(json_content(y))
    //alias_node ~/ Fail |    
    //TODO: in salar flow_content | ( y.properties ~ P( ( y.separate ~ flow_content ) | e_scalar ) )

  // [160]   c-flow-json-node(n,c)              ::= ( c-ns-properties(n,c) s-separate(n,c) )? c-flow-json-content(n,c)
  def json_node(y:Y):Parser[T] = (y.properties ~ y.separate).? ~ json_content(y)
}


/*

abstract class ObjectElement[T :< Product] extends Element[T]{
def map[T](f:T=>O)
}

Object1[(P1)](
 p1:(String, Element[P1])
){
def map[T](f:T1=>T)
}
Object2[P1,P2](
 p1:(String, Element[P1])
 p2:(String, Element[P2])
){
def map[T](f:T1,T2=>T)
}
 */
