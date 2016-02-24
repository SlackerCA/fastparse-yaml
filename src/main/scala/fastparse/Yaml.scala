package fastparse.yaml

object Yaml {
  def Map(pairs:(Element[K], Element[T])*):MapElement[Map[K,T]]
}


abstract class Element[T] {
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


abstract class Parsers[T] {
  def block_node(y:Y)
  def block_indented(y:Y)
  def e_scalar = Fail
  def json_content(y:Y)

  // [159]   ns-flow-yaml-node(n,c)             ::= c-ns-alias-node | ns-flow-yaml-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-yaml-content(n,c) ) | e-scalar ) )
  private def yaml_node(y:Y) = {
    val flow_yaml_content = yaml_content(y)
    //y.alias_node ~/ Fail |
    yaml_content | ( y.properties ~ P( ( y.separate ~ yaml_content ) | e_scalar ) )
  }

  // [160]   c-flow-json-node(n,c)              ::= ( c-ns-properties(n,c) s-separate(n,c) )? c-flow-json-content(n,c)
  private def json_node(y:Y) = ( y.properties ~ y.separate )? json-content(y)
}

class MapParsers[K,V] (val pairs:Seq[(Element[K], Element[V])]) extends Parsers[(K,V)]{
    //[196]   s-l+block-node(n,c)                ::= s-l+block-in-block(n,c) | s-l+flow-in-block(n)
    // [198]   s-l+block-in-block(n,c)            ::= s-l+block-scalar(n,c) | s-l+block-collection(n,c)
    //  [200]   s-l+block-collection(n,c)          ::= ( s-separate(n+1,c) c-ns-properties(n+1,c) )? s-l-comments ( l+block-sequence(seq-spaces(n,c)) | l+block-mapping(n) )
  def block_node(y:Y) = {
    val y1 = Y(y.indent+1,y.context)
    val seq_indent = if(y.context == BlockOut) y.indent-1 else y.indent

    //not block-scalar
    !( y1.separate ~ (y1.properties ~ y1.separate).? ~ CharIn("|<") ) ~
      ( y1.separate ~ y1.properties ).? ~ comments ~
    y.indent_more.flatMap((i:Int) => block_mapping(Y(i,y.context)))
  }
  // compact_mapping works out to the same as block_mapping
  def block_indented(y:Y) = indent_any.flatMap((i:Int) => block_mapping(Y(y.indent+1+i,y.context))) | block_node(y) //| ( e_node s-l-comments )

  private val not_block_seq_entry =  !("-" ~ CharIn(" \t\n\l"))
  //no-cache
  private def block_collection(y:Y) = y.indent_more.flatMap(n=>block_mapping(Y(n,y.context)))

  //c-l-block-seq-entry(n)             ::= “-” /* Not followed by an ns-char */
  private def block_mapping(y:Y) = !("-" ~ CharIn(" \t\n\l")) ~ block_map_entry(y).rep(min=1,delim=y.indent)

  private def block_map_entry(y:Y) = (
    "?" ~/ flatMap(block_map_explicit_entry(y)),

  )

  // [189]   c-l-block-map-explicit-entry(n)    ::= c-l-block-map-explicit-key(n) ( l-block-map-explicit-value(n) | e-node )
  //  [190]   c-l-block-map-explicit-key(n)      ::= “?” s-l+block-indented(n,block-out)
  //  [191]   l-block-map-explicit-value(n)      ::= s-indent(n) “:” s-l+block-indented(n,block-out)
  private def block_map_explicit_entry(y:Y) = {
    val yBlockOut = Y(y.indent,BlockOut)
    Either(
      for((key,value) <- pairs) yield
        (key.block_indented(yBlockOut) ~ ":" ~/ value.block_indented(yBlockOut))
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


}

class ListParsers[T]{
  def block_node(n:Int,c:Context) = {

  }
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
