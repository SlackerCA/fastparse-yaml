package fastparse.yaml

import fastparse.all._

class ScalarElement(val filter:(String => Boolean)) extends Element[String] {
  import YamlParsers._

  // [185]   s-l+block-indented(n,c)            ::= ( s-indent(m) ( ns-l-compact-sequence(n+1+m) | ns-l-compact-mapping(n+1+m) ) ) | s-l+block-node(n,c) | ( e-node s-l-comments )
  //  [186]   ns-l-compact-sequence(n)           ::= c-l-block-seq-entry(n) ( s-indent(n) c-l-block-seq-entry(n) )*
  //  [195]   ns-l-compact-mapping(n)            ::= ns-l-block-map-entry(n) ( s-indent(n) ns-l-block-map-entry(n) )*
  //override def block_indented(y:Y)// = y.block_indented.filter(filter).!

  override val e_node = (if(filter("")) Pass else Fail).map(Unit=>"")

  override def yaml_content(y:Y):Parser[String] = y.plain.filter(filter)

  override def json_content(y:Y):Parser[String] = y.quoted.filter(filter)

  // [159]   ns-flow-yaml-node(n,c)             ::= c-ns-alias-node | ns-flow-yaml-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-yaml-content(n,c) ) | e-scalar ) )
  override def yaml_node(y:Y):Parser[String] = {
    val yaml_content_y = yaml_content(y)
    yaml_content_y | ( y.properties ~ ( ( y.separate ~ yaml_content_y ) | e_node ) )
  }
    //y.alias_node ~/ Fail |
    //TODO: in scalar flow_yaml_content | ( y.properties ~ P( ( y.separate ~ flow_yaml_content ) | e_scalar ) )


  // [196]   s-l+block-node(n,c)                ::= s-l+block-in-block(n,c) | s-l+flow-in-block(n)
  //  [198]   s-l+block-in-block(n,c)            ::= s-l+block-scalar(n,c) | s-l+block-collection(n,c)
  //   [199]   s-l+block-scalar(n,c)              ::= s-separate(n+1,c) ( c-ns-properties(n+1,c) s-separate(n+1,c) )? ( c-l+literal(n) | c-l+folded(n) )
  //    [170]   c-l+literal(n)                     ::= “|” c-b-block-header(m,t) l-literal-content(n+m,t)
  //     [173]   l-literal-content(n,t)             ::= ( l-nb-literal-text(n) b-nb-literal-next(n)* b-chomped-last(t) )? l-chomped-empty(n,t)
  override def block_node(y:Y) = {
    y.block_scalar | P(flow_in_block(YamlParser(y.indentation+1,FlowOut)))
  }
  //  [197]   s-l+flow-in-block(n)               ::= s-separate(n+1,flow-out) ns-flow-node(n+1,flow-out) s-l-comments
  private def flow_in_block(y:Y) = (y.separate ~ flow_node(y) ~ comments)//.log("flow_in_block")

  // [161]   ns-flow-node(n,c)                  ::= c-ns-alias-node | ns-flow-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-content(n,c) ) | e-scalar ) )
  //  [158]   ns-flow-content(n,c)               ::= ns-flow-yaml-content(n,c) | c-flow-json-content(n,c)
  //override def flow_node(y:Y):Parser[String]// = y.flow_node.filter(filter).!
  override def flow_node(y:Y):Parser[String] = {
    val flow_content = yaml_content(y) | P(json_content(y))
    alias_node | flow_content | P( y.properties ~ ( ( y.separate ~ flow_content ) | e_scalar ) )
  }
}


