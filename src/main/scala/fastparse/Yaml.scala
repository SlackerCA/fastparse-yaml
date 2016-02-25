package fastparse.yaml

/*

=Design Goals=
- easy to learn, ideal for one time use
 - no special syntax
 - use Scala types, not rather then YAML
 - hide/ignore YAML "features"
- simple for the default case
 - order of list and map entries does not matter
 - tuple for a short list of fixed size
- mostly used for short one time parse
 - performance is a lower priority


=Maps=
Maps form the backbone as parser only branches on map keys.

Map(
(
"foo" -> string,
"bar" -> mapOf(string->string).map(m:Map[String,String]=>new Foo(m)),
"annotations" -> collect("(.*)".r -> annotation) //Map[String,Annotation]
collect("\\?.*)".r -> optional)


).map(m:((String,String),(String,Foo)),(Map[String,Annotation])=> Obj(m._1._2,m._2._2)
.map(match{ case (("foo",foo:String),("bar",bar:Foo)) => Obj(foo,bar) })

def collect[K,V](pair:(K,V)):Map[K,V]


ObjectMap("foo","bar")(string, listOf(..))
.map(Obj.tupled)

ObjectMap("foo","bar")(string, listOf(..))


class ObjectMap[F1,F2] (
((String,F1),(String,F2))
){
def map[T](f:(F1,F2)=>T
}
((String,F1),(String,F2)) => (F1,F2)
((String,F1),(String,F2),(String,F3)) => (F1,F2,F3)


class Node[T](t:T){
 def map[V](f:T=>V):V = f(t)
}

def Map[T](m:T):Node[T] = new Node(m)

=Lists=
listOf(n:Node) => list(n.rep())


=Scalar patterns=
string(prefix:String="",suffix:String="")
""
"".r
String => Boolean
Parser

=flatMap=


optional()
either() can not mix types (simple)
anyof() can mix types (complex)


=======
just produce case classes (list,map,scalar) for second pass parsing
parse() calls match

Yaml(
"title"->string,
"documentation" -> listOf(map...)
"annotations"-> map(
(string -> map)(match { case (k:String,v:Map) if k.startsWith("/") => (k,v)})
)()

) match {
case m:Map => // val Raml(m.get("title"),m.get("descirption"),m.get("annotations").map(
}

val resource = map("type" -> , string -> httpmethod)

object(""-> , string -> fn )



*/

/*

Parser {
call parseReq
on success

}

 */
/*
object Yaml {
  def Map(pairs:(Element[K], Element[T])*):MapElement[Map[K,T]]
}


abstract class Element[T] {
  parser: Parser[T] =>
  def map[B](f:T=>B):Element[B]
  def filter(f:T=>Boolean):Element[T]
  def parse(so:String):T = {
    block_node(-1,block_in).parse(s)
  }
}

//class MappedElement (e:Element[])

//def each(pairs:Seq[Parser[(K,V)]])

//extend Parser. loop

/**
  * All elements are optional, but at least one must match.
  * Use post processing to check for required fields.
  */
class MapElement[K,V] (val pairs:Seq[(Element[K], Element[V])]) extends Element[(K,V)] {
  def map[B](f:T=>B):Element[B]
  private[fastparse.yaml] val parser = new MapParsers(pairs)
}

 */
abstract class Parsers[T] {
  def block_node(y:Y)
  // [185]   s-l+block-indented(n,c)            ::= ( s-indent(m) ( ns-l-compact-sequence(n+1+m) | ns-l-compact-mapping(n+1+m) ) ) | s-l+block-node(n,c) | ( e-node s-l-comments )
  //  [186]   ns-l-compact-sequence(n)           ::= c-l-block-seq-entry(n) ( s-indent(n) c-l-block-seq-entry(n) )*
  //  [195]   ns-l-compact-mapping(n)            ::= ns-l-block-map-entry(n) ( s-indent(n) ns-l-block-map-entry(n) )*
  def block_indented(y:Y)
  def e_scalar = Fail
  def yaml_content(y:Y)
  def json_content(y:Y)

  // [159]   ns-flow-yaml-node(n,c)             ::= c-ns-alias-node | ns-flow-yaml-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-yaml-content(n,c) ) | e-scalar ) )
  private def yaml_node(y:Y) = {
    val flow_yaml_content = yaml_content(y)
    //y.alias_node ~/ Fail |
    yaml_content | ( y.properties ~ P( ( y.separate ~ yaml_content ) | e_scalar ) )
  }

  // [160]   c-flow-json-node(n,c)              ::= ( c-ns-properties(n,c) s-separate(n,c) )? c-flow-json-content(n,c)
  private def json_node(y:Y) = ( y.properties ~ y.separate )? json-content(y)

  // [161]   ns-flow-node(n,c)                  ::= c-ns-alias-node | ns-flow-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-content(n,c) ) | e-scalar ) )
  //  [158]   ns-flow-content(n,c)               ::= ns-flow-yaml-content(n,c) | c-flow-json-content(n,c)
  private def flow_node(y:Y) = {
    val flow_content = yaml_content(y) | P(json_content(y))
    //alias_node ~/ Fail |
    flow_content | ( y.properties ~ P( ( y.separate ~ flow_content ) | e_scalar ) )
  }
}

class MapParsers[K,V] (val pairs:Seq[(Element[K], Element[V])]) extends Parsers[(K,V)]{
    //[196]   s-l+block-node(n,c)                ::= s-l+block-in-block(n,c) | s-l+flow-in-block(n)
    // [198]   s-l+block-in-block(n,c)            ::= s-l+block-scalar(n,c) | s-l+block-collection(n,c)
    //  [200]   s-l+block-collection(n,c)          ::= ( s-separate(n+1,c) c-ns-properties(n+1,c) )? s-l-comments ( l+block-sequence(seq-spaces(n,c)) | l+block-mapping(n) )
    // [197]   s-l+flow-in-block(n)               ::= s-separate(n+1,flow-out) ns-flow-node(n+1,flow-out) s-l-comments
  def block_node(y:Y) = {
    val y1 = Y(y.indent+1,y.context)
    val seq_indent = if(y.context == BlockOut) y.indent-1 else y.indent
    
    val not_block_scalar = !( y1.separate ~ (y1.properties ~ y1.separate).? ~ CharIn("|<") ) // cache me
    val not_block_seq_entry =  !(y.indent_more ~ "-" ~ CharIn(" \t\n\r")) // cache me
    val block_in_block = 
      not_block_scalar ~
      ( y1.separate ~ y1.properties ).? ~ comments ~ 
      not_block_seq_entry ~
      y.indent_more.flatMap((i:Int) => block_mapping(Y(i,y.context)))

    val y1FO = Y(y.indent+1,FlowOut)
    val flow_in_block = y1FO.separate ~ flow_node(y1FO) ~ comments

    block_in_block | flow_in_block
  }


  // [185]   s-l+block-indented(n,c)            ::= ( s-indent(m) ( ns-l-compact-sequence(n+1+m) | ns-l-compact-mapping(n+1+m) ) ) | s-l+block-node(n,c) | ( e-node s-l-comments )
  //  [186]   ns-l-compact-sequence(n)           ::= c-l-block-seq-entry(n) ( s-indent(n) c-l-block-seq-entry(n) )*
  //  [195]   ns-l-compact-mapping(n)            ::= ns-l-block-map-entry(n) ( s-indent(n) ns-l-block-map-entry(n) )*
  def block_indented(y:Y) = {
    val not_block_seq_entry =  !(y.indent_any ~ "-" ~ CharIn(" \t\n\r")) // cache me
    //compact-sequence(n)           ::= c-l-block-seq-entry(n)
    // our modifed block-mapping works for compact-mapping
    indent_any.flatMap((i:Int) => block_mapping(Y(y.indent+1+i,y.context))) | block_node(y) //| ( e_node s-l-comments )
  }

  // [187]   l+block-mapping(n)                 ::= ( s-indent(n+m) ns-l-block-map-entry(n+m) )+ /* For some fixed auto-detected m > 0 */
  private def block_mapping(y:Y) = block_map_entry(y).rep(min=1,sep=y.s_indent)

  // [188]   ns-l-block-map-entry(n)            ::= c-l-block-map-explicit-entry(n) | ns-l-block-map-implicit-entry(n)
  private def block_map_entry(y:Y) = block_map_explicit_entry(y) | block_map_implicit_entry(y)


  // [189]   c-l-block-map-explicit-entry(n)    ::= c-l-block-map-explicit-key(n) ( l-block-map-explicit-value(n) | e-node )
  //  [190]   c-l-block-map-explicit-key(n)      ::= “?” s-l+block-indented(n,block-out)
  //  [191]   l-block-map-explicit-value(n)      ::= s-indent(n) “:” s-l+block-indented(n,block-out)
  private def block_map_explicit_entry(y:Y) = {
    val yBlockOut = Y(y.indent,BlockOut)
    "?" ~/ P(
      Either (
        for((key,value) <- pairs) yield
          (key.block_indented(yBlockOut) ~ ":" ~/ value.block_indented(yBlockOut))
      )
    )
  }

  // [192]   ns-l-block-map-implicit-entry(n)   ::= ( ns-s-block-map-implicit-key | e-node ) c-l-block-map-implicit-value(n)
  //  [193]   ns-s-block-map-implicit-key        ::= c-s-implicit-json-key(block-key) | ns-s-implicit-yaml-key(block-key)
  //  [194]   c-l-block-map-implicit-value(n)    ::= “:” ( s-l+block-node(n,block-out) | ( e-node s-l-comments ) )
  private def block_map_implicit_entry(y:Y) = {
    val yBlockKey = Y(y.indent,BlockKey)
    val yBlockOut = Y(y.indent,BlockOut)
    val sep = (":" ~ &(CharIn(" \t\r\n"))).~/
    Either(
      for((key, value) <- pairs) yield
        //(implicit_json_key(key,yBlockKey) | implicit_yaml_key(key,yBlockKey)) ~ ":" ~/  value.block_node(yBlockOut)
        (key.json_node(y) | key.yaml_node(y)) ~ y.separate_in_line.?  ~ sep  ~ value.block_node(yBlockOut)
    )
  }

  // [154]   ns-s-implicit-yaml-key(c)          ::= ns-flow-yaml-node(n/a,c) s-separate-in-line? /* At most 1024 characters altogether */
  //private def implicit_yaml_key(elem:Element[T], y:Y) = elem.yaml_node(y) ~ y.separate_in_line.?

  // [155]   c-s-implicit-json-key(c)           ::= c-flow-json-node(n/a,c) s-separate-in-line? /* At most 1024 characters altogether */
  //private def implicit_json_key(elem:Element[T], y:Y) = elem.json_node(y) ~ y.separate_in_line.?

  // [161]   ns-flow-node(n,c)                  ::= c-ns-alias-node | ns-flow-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-content(n,c) ) | e-scalar ) )
  //  [158]   ns-flow-content(n,c)               ::= ns-flow-yaml-content(n,c) | c-flow-json-content(n,c)
  private def flow_node(y:Y) = {
    // only json-content can be a map, and yaml-content can not start with "{", so no need for negative lookahead
    val flow_content = json_content(y)
    //alias_node ~/ Fail |
    flow_content | ( y.properties ~ P( ( y.separate ~ flow_content ) | e_scalar ) )
  }

  def yaml_content(y:Y) = Fail

  // [157]   c-flow-json-content(n,c)           ::= c-flow-sequence(n,c) | c-flow-mapping(n,c) | c-single-quoted(n,c) | c-double-quoted(n,c)
  //  [140]   c-flow-mapping(n,c)                ::= “{” s-separate(n,c)? ns-s-flow-map-entries(n,in-flow(c))? “}”
  //   [141]   ns-s-flow-map-entries(n,c)         ::= ns-flow-map-entry(n,c) s-separate(n,c)? ( “,” s-separate(n,c)? ns-s-flow-map-entries(n,c)? )?
  //    [142]   ns-flow-map-entry(n,c)             ::= ( “?” s-separate(n,c) ns-flow-map-explicit-entry(n,c) ) | ns-flow-map-implicit-entry(n,c)
  def json_content(y:Y) = "{" ~/ y.separate.? ~ P(flow_map_entries(Y(y.indent,y.in_flow))).?  ~ "}"
  private def flow_map_entries(y:Y) = {
    val flow_map_implicit_entry = flow_map_implicit_entry(y)
    (( "?" ~ y.separate ~/ flow_map_implicit_entry //| (e_node.! ~ e_node.!) TODO: support for matching on empty key and empty value
                                                  ) | flow_map_implicit_entry)
      .rep(min=1,sep=(y.separate.? ~ "," ~ y.separate.?))
  }

  // [143]   ns-flow-map-explicit-entry(n,c)    ::= ns-flow-map-implicit-entry(n,c) | ( e-node /* Key */ e-node /* Value */ )
  // [144]   ns-flow-map-implicit-entry(n,c)    ::= ns-flow-map-yaml-key-entry(n,c) | c-ns-flow-map-empty-key-entry(n,c) | c-ns-flow-map-json-key-entry(n,c)
  //  [145]   ns-flow-map-yaml-key-entry(n,c)    ::= ns-flow-yaml-node(n,c) ( ( s-separate(n,c)? c-ns-flow-map-separate-value(n,c) ) | e-node )
  //  [148]   c-ns-flow-map-json-key-entry(n,c)  ::= c-flow-json-node(n,c) ( ( s-separate(n,c)? c-ns-flow-map-adjacent-value(n,c) ) | e-node )
  private def flow_map_implicit_entry(y:Y) = {
    Either(
      for((key, value) <- pairs) yield {
        // [147]   c-ns-flow-map-separate-value(n,c)  ::= ":" /* Not followed by an ns-plain-safe(c) */ ( ( s-separate(n,c) ns-flow-node(n,c) ) | e-node /* Value */ )
        val flow_map_separate_value = ":" ~ !y.plain_safe ~/ ( ( y.separate ~ value.flow_node(y) ) | value.e_node )
        //ns_flow_map_yaml_key_entry
        (key.yaml_node(y) ~ ( ( y.separate.? ~ flow_map_separate_value ) | value.e_node )) |
        //c-ns-flow-map-empty-key-entry(n,c)
        P(key.e_node ~ flow_map_separate_value ) |
        // c-ns-flow-map-json-key-entry(n,c)
        P(key.json_node ~/ ( ( s_separate.? ~ ":" ~/ ( ( y.separate.? ~ value.flow_node(y) ) | value.e_node ) ) | value.e_node ))
      }
    )
  }
}

// [196]   s-l+block-node(n,c)                ::= s-l+block-in-block(n,c) | s-l+flow-in-block(n)
//  [198]   s-l+block-in-block(n,c)            ::= s-l+block-scalar(n,c) | s-l+block-collection(n,c)
//   [199]   s-l+block-scalar(n,c)              ::= s-separate(n+1,c) ( c-ns-properties(n+1,c) s-separate(n+1,c) )? ( c-l+literal(n) | c-l+folded(n) )
//    [170]   c-l+literal(n)                     ::= “|” c-b-block-header(m,t) l-literal-content(n+m,t)
//     [173]   l-literal-content(n,t)             ::= ( l-nb-literal-text(n) b-nb-literal-next(n)* b-chomped-last(t) )? l-chomped-empty(n,t)
/*
class ScalarParsers[T]{
  def block_node(y:Y) = {
    val y1 = Y(y.indent+1,y.context)
    val block_scalar = y.separate ( y1.properties ~ y1.separate )? ( literal(y) | c-l+folded(n) )

  }

  def block_indented(y:Y) = {}
  def yaml_content(y:Y) = {}
  def json_content(y:Y) = {}
}

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
