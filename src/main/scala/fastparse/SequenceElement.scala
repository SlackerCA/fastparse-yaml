package fastparse.yaml

import fastparse.all._

class SequenceElement[T] (val elements:Seq[Element[T]]) extends Element[Seq[T]]{
  import YamlParsers._

  //[196]   s-l+block-node(n,c)                ::= s-l+block-in-block(n,c) | s-l+flow-in-block(n)
  // [198]   s-l+block-in-block(n,c)            ::= s-l+block-scalar(n,c) | s-l+block-collection(n,c)
  //  [200]   s-l+block-collection(n,c)          ::= ( s-separate(n+1,c) c-ns-properties(n+1,c) )? s-l-comments ( l+block-sequence(seq-spaces(n,c)) | l+block-mapping(n) )
  // [197]   s-l+flow-in-block(n)               ::= s-separate(n+1,flow-out) ns-flow-node(n+1,flow-out) s-l-comments
  override def block_node(y:Y) = {
    val not_block_scalar = !( y.block_scalar_preamble )
    //val not_block_seq_entry =  !(y.indent_more ~ "-" ~ CharIn(" \t\n\r")) // cache me
    val block_in_block =
      not_block_scalar ~
      y.block_collection_preamble ~
      Peek(y.seq_spaces ~ "-" ~ s_char).flatMap((n:Int)=>block_sequence(y(n)))

    val y1FO = YamlParser(y.indentation+1,FlowOut)
    val flow_in_block = y1FO.separate ~ flow_node(y1FO) ~ comments

    block_in_block | flow_in_block
  }

  private def block_sequence(y:Y):Parser[Seq[T]] = {
    val yBlockIn = y(BlockIn)
    (y.indent ~ "-" ~ s_char ~ either( 
      for(element <- elements) yield (element.block_indented(yBlockIn))
    )).rep(1)
  }

/*

  // [185]   s-l+block-indented(n,c)            ::= ( s-indent(m) ( ns-l-compact-sequence(n+1+m) | ns-l-compact-mapping(n+1+m) ) ) | s-l+block-node(n,c) | ( e-node s-l-comments )
  //  [186]   ns-l-compact-sequence(n)           ::= c-l-block-seq-entry(n) ( s-indent(n) c-l-block-seq-entry(n) )*
  //  [195]   ns-l-compact-mapping(n)            ::= ns-l-block-map-entry(n) ( s-indent(n) ns-l-block-map-entry(n) )*
  override def block_indented(y:Y) = {
    val not_block_seq_entry =  !(indent_any ~ "-" ~ CharIn(" \t\n\r")) // cache me
    //compact-sequence(n)           ::= c-l-block-seq-entry(n)
    // our modifed block-mapping works for compact-mapping
    indent_any.flatMap((i:Int) => block_mapping(y+1)) | block_node(y) //| ( e_node s-l-comments )
  }

  // [187]   l+block-mapping(n)                 ::= ( s-indent(n+m) ns-l-block-map-entry(n+m) )+ /* For some fixed auto-detected m > 0 */
  private def block_mapping(y:Y):Parser[Map[K,V]] = block_map_entry(y).rep(min=1,sep=y.indent).map(_.toMap)

  // [188]   ns-l-block-map-entry(n)            ::= c-l-block-map-explicit-entry(n) | ns-l-block-map-implicit-entry(n)
  private def block_map_entry(y:Y) = block_map_explicit_entry(y) | block_map_implicit_entry(y)


  // [189]   c-l-block-map-explicit-entry(n)    ::= c-l-block-map-explicit-key(n) ( l-block-map-explicit-value(n) | e-node )
  //  [190]   c-l-block-map-explicit-key(n)      ::= “?” s-l+block-indented(n,block-out)
  //  [191]   l-block-map-explicit-value(n)      ::= s-indent(n) “:” s-l+block-indented(n,block-out)
  private def block_map_explicit_entry(y:Y):Parser[(K,V)] = {
    val yBlockOut = y(BlockOut)
    "?" ~/ P(
      Either ( 
        for((key,value) <- pairs)
        yield (key.block_indented(yBlockOut) ~ ":" ~/ value.block_indented(yBlockOut))
      )
    )
  }

  // [192]   ns-l-block-map-implicit-entry(n)   ::= ( ns-s-block-map-implicit-key | e-node ) c-l-block-map-implicit-value(n)
  //  [193]   ns-s-block-map-implicit-key        ::= c-s-implicit-json-key(block-key) | ns-s-implicit-yaml-key(block-key)
  //  [194]   c-l-block-map-implicit-value(n)    ::= “:” ( s-l+block-node(n,block-out) | ( e-node s-l-comments ) )
  // [154]   ns-s-implicit-yaml-key(c)          ::= ns-flow-yaml-node(n/a,c) s-separate-in-line? /* At most 1024 characters altogether */
  // [155]   c-s-implicit-json-key(c)           ::= c-flow-json-node(n/a,c) s-separate-in-line? /* At most 1024 characters altogether */
  private def block_map_implicit_entry(y:Y):Parser[(K,V)] = {
    val yBlockKey = y(BlockKey)
    val yBlockOut = y(BlockOut)
    val sep = (":" ~ &(CharIn(" \t\r\n"))).~/
    Either(
      for((key, value) <- pairs) 
      yield (key.json_node(yBlockKey) | key.yaml_node(yBlockKey)) ~ separate_in_line.?  ~ sep  ~ value.block_node(yBlockOut)
    )
  }

  // [161]   ns-flow-node(n,c)                  ::= c-ns-alias-node | ns-flow-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-content(n,c) ) | e-scalar ) )
  //  [158]   ns-flow-content(n,c)               ::= ns-flow-yaml-content(n,c) | c-flow-json-content(n,c)
  // only json-content can be a map, and yaml-content can not start with "{", so no need for negative lookahead
  override def flow_node(y:Y) = (y.properties ~ y.separate).? ~ json_content(y)

  override def yaml_content(y:Y) = Fail

  // [157]   c-flow-json-content(n,c)           ::= c-flow-sequence(n,c) | c-flow-mapping(n,c) | c-single-quoted(n,c) | c-double-quoted(n,c)
  //  [140]   c-flow-mapping(n,c)                ::= “{” s-separate(n,c)? ns-s-flow-map-entries(n,in-flow(c))? “}”
  //   [141]   ns-s-flow-map-entries(n,c)         ::= ns-flow-map-entry(n,c) s-separate(n,c)? ( “,” s-separate(n,c)? ns-s-flow-map-entries(n,c)? )?
  //    [142]   ns-flow-map-entry(n,c)             ::= ( “?” s-separate(n,c) ns-flow-map-explicit-entry(n,c) ) | ns-flow-map-implicit-entry(n,c)
  //TODO: match on empty map
  override def json_content(y:Y) = "{" ~/ y.separate.? ~ P(flow_map_entries(y(y.in_flow)))  ~ "}"

  private def flow_map_entries(y:Y):Parser[Map[K,V]] = {
    val implicit_entry = flow_map_implicit_entry(y)
    (( "?" ~ y.separate ~/ implicit_entry //| (e_node.! ~ e_node.!) TODO: support for matching on empty key and empty value
                                                  ) | implicit_entry)
      .rep(min=1,sep=(y.separate.? ~ "," ~ y.separate.?)).map(_.toMap)
  }

  // [143]   ns-flow-map-explicit-entry(n,c)    ::= ns-flow-map-implicit-entry(n,c) | ( e-node /* Key */ e-node /* Value */ )
  // [144]   ns-flow-map-implicit-entry(n,c)    ::= ns-flow-map-yaml-key-entry(n,c) | c-ns-flow-map-empty-key-entry(n,c) | c-ns-flow-map-json-key-entry(n,c)
  //  [145]   ns-flow-map-yaml-key-entry(n,c)    ::= ns-flow-yaml-node(n,c) ( ( s-separate(n,c)? c-ns-flow-map-separate-value(n,c) ) | e-node )
  //  [148]   c-ns-flow-map-json-key-entry(n,c)  ::= c-flow-json-node(n,c) ( ( s-separate(n,c)? c-ns-flow-map-adjacent-value(n,c) ) | e-node )
  private def flow_map_implicit_entry(y:Y) = {
    Either(
      for((key, value) <- pairs) 
      yield {
        // [147]   c-ns-flow-map-separate-value(n,c)  ::= ":" /* Not followed by an ns-plain-safe(c) */ ( ( s-separate(n,c) ns-flow-node(n,c) ) | e-node /* Value */ )
        val flow_map_separate_value = ":" ~ !y.plain_safe ~/ ( ( y.separate ~ value.flow_node(y) ) | value.e_node )
        //ns_flow_map_yaml_key_entry
        (key.yaml_node(y) ~ ( ( y.separate.? ~ flow_map_separate_value ) | value.e_node )) |
        //c-ns-flow-map-empty-key-entry(n,c)
        P(key.e_node ~ flow_map_separate_value ) |
        // c-ns-flow-map-json-key-entry(n,c)
        P(key.json_node(y) ~/ ( ( y.separate.? ~ ":" ~/ ( ( y.separate.? ~ value.flow_node(y) ) | value.e_node ) ) | value.e_node ))
      }
    )
  }
 */
}
