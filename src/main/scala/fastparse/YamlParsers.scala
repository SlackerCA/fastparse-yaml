package fastparse.yaml

import fastparse.all._

private class Context
private object BlockOut extends Context
private object BlockIn extends Context
private object FlowOut extends Context
private object FlowIn extends Context
private object BlockKey extends Context
private object FlowKey extends Context

object YamlParser extends YamlParser(0,FlowIn)
/** 
 * No UTF-32 encodings support.
 * @see http://www.yaml.org/spec/1.2/spec.html
  */
//TODO: move indentation aware parsers to subclass
class YamlParser private (private val indentation:Int, private val context:Context) {

  //def between(from:Char,to:Char)(c:Char) = from <= c && c <= to

  //('\u00A0' <= c && c <= '\uD7FF') || ('\uE000' <= c && c <= '\uFFFD')

  // [1]        c-printable     ::=       #x9 | #xA | #xD | [#x20-#x7E]          /* 8 bit */
  //                                    | #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD] /* 16 bit */
  //                                    | [#x10000-#x10FFFF]                     /* 32 bit */
  val c_printable = P(CharIn(List('\u0009','\u000A','\u000D','\u0085'), ('\u0020' to '\u007E'), ('\u00A0' to '\uD7FF'), ('\uE000' to '\uFFFD')))
  // the high end of c-printable which is usually included
  //val c_printable_high = CharPred(c => ('\u00A0' <= c && c <= '\uD7FF') || ('\uE000' <= c && c <= '\uFFFD') || c == '\u0085')

  //[2] nb-json ::=     #x9 | [#x20-#x10FFFF]
  val nb_json = P(CharIn(List('\u0009'), '\u0020' to '\uFFFF'))

  // [3]        c-byte-order-mark       ::=     #xFEFF
  val c_byte_order_mark = "\uFEFF"
  // [22]       c-indicator     ::=       “-” | “?” | “:” | “,” | “[” | “]” | “{” | “}” | “#” | “&” | “*” | “!” | “|” | “>” | “'” | “"” | “%” | “@” | “`”
  val c_indicator = P(CharIn("-?:,[]{}#&*!|>'\"%@`"))
  // [23]       c-flow-indicator        ::=     “,” | “[” | “]” | “{” | “}”
  val c_flow_indicator = P(CharIn("-[]{}"))

  // [24]       b-line-feed     ::=     #xA    /* LF */
  // [25]       b-carriage-return       ::=     #xD    /* CR */
  // [26]       b-char  ::=     b-line-feed | b-carriage-return
  val b_line_feed = P("\u000A")
  val b_carriage_return = P("\u000D")
  val b_char = P(b_line_feed | b_carriage_return)

  // [27]       nb-char ::=     c-printable - b-char - c-byte-order-mark
  //val nb_char = P(!(b_char|c_byte_order_mark) ~ c_printable)
  // c-printable - b-line-feed - b-carriage-return - c-byte-order-mark
  val nb_char = P(CharIn(List('\u0009', '\u0085'), ('\u00A0' to '\uD7FF'), ('\uE000' to '\uFFFD').diff(c_byte_order_mark)))
  // [28]       b-break ::=       ( b-carriage-return b-line-feed ) | b-carriage-return | b-line-feed
  val b_break = P((b_carriage_return ~ b_line_feed) | b_carriage_return | b_line_feed)
  // [29]       b-as-line-feed  ::=     b-break
  val b_as_line_feed = P(b_break.map(_ => "\u000A"))


  //val nb_char = P(!(b_char|c_byte_order_mark) ~ c_printable)
  val nv_char = !CharIn(List('\u0009', '\u000A', '\u000D','\u0085'),('\u00A0' to '\uD7FF'), ('\uE000' to '\uFFFD'))
 

  // [33]       s-white ::=     s-space | s-tab
  val s_white = P(CharIn(" \\t"))

  // [34]       ns-char ::=     nb-char - s-white
  // c-printable - b-line-feed - b-carriage-return - c-byte-order-mark - s-space - s-tab
  val ns_char = P(CharIn(('\u0021' to '\u007E'), ('\u00A0' to '\uD7FF'), ('\uE000' to '\uFFFD')))

  // [62]       c-ns-esc-char   ::=     “\” /* followed by one of ns-esc-* */
  private val ns_esc_map = Map(
    "\\0" -> '\u0000',
    "\\a" -> '\u0007',
    "\\b" -> '\u0008',
    "\\t" -> '\u0009'
    //TODO: map all escaped chars to values
  )
  private val escaped = ("\\" ~ CharIn("0abt\tnvfre \"/\\N_LP").!.map(ns_esc_map.get(_)))
  private val hexDigit = CharIn("abcdefABCDEF0123456789")
  private val unicode = ("\\x" ~ hexDigit.rep(min=2,max=2).!.map(Integer.parseInt(_, 4)) | "\\u" ~ hexDigit.rep(min=4,max=4).!.map(Integer.parseInt(_, 8)) /*| "\\U" ~ hexDigit(8)*/)
  val c_ns_esc_char = (escaped | unicode)

  // [63]       s-indent(n)     ::=     s-space × n
  val s_indent = P(" ".rep(indentation))
  // [64]       s-indent(<n)    ::=     s-space × m /* Where m < n */
  val s_indent_lt = P(" ".rep)
  // [65]       s-indent(≤n)    ::=     s-space × m /* Where m ≤ n */

  // [66]       s-separate-in-line      ::=     s-white+ | /* Start of line */
  val s_separate_in_line = s_white.rep(1) | Start

  // [67]       s-line-prefix(n,c)      ::=     c = block-out ⇒ s-block-line-prefix(n)
  //                                            c = block-in  ⇒ s-block-line-prefix(n)
  //                                            c = flow-out  ⇒ s-flow-line-prefix(n)
  //                                            c = flow-in   ⇒ s-flow-line-prefix(n)    
  // [68]       s-block-line-prefix(n)  ::=     s-indent(n)
  // [69]       s-flow-line-prefix(n)   ::=     s-indent(n) s-separate-in-line?
  val s_block_line_prefix = P(s_indent)
  val s_flow_line_prefix = P(s_indent) ~ s_separate_in_line.?
  val s_line_prefix = P(Pass.flatMap(unit => this.context match {
    case BlockOut => s_block_line_prefix
    case BlockIn  => s_block_line_prefix
    case FlowOut  => s_flow_line_prefix
    case FlowIn   => s_flow_line_prefix
  }))
  

  // [75]       c-nb-comment-text       ::=     “#” nb-char*
  // [76]               b-comment       ::=     b-non-content | /* End of file */
  // [77]             s-b-comment       ::=     ( s-separate-in-line c-nb-comment-text? )? b-comment
  // ends a line in non scalar context 
  val s_b_comment = P(((Start|s_white.rep(1)) ~ "#" ~ nb_char.rep).? ~ (b_break|End))
  // [78]               l-comment       ::=     s-separate-in-line c-nb-comment-text? b-comment
  val l_comment = P((Start|s_white.rep(1)) ~ ("#" ~ nb_char.rep).? ~ (b_break|End))
  // [79]            s-l-comments       ::=     ( s-b-comment | /* Start of line */ ) l-comment*
  val s_l_comments = P((s_b_comment|Start) ~ l_comment.rep)


  // [96]       c-ns-properties(n,c)    ::=       ( c-ns-tag-property ( s-separate(n,c) c-ns-anchor-property )? ) | ( c-ns-anchor-property ( s-separate(n,c) c-ns-tag-property )? )
  //TODO: implment node properties
  //val c_ns_properties = P( ( c_ns_tag_property ~ ( s_separate ~ c_ns_anchor_property )? ) | ( c_ns_anchor_property ~ ( s_separate c_ns_tag_property )? ) )

  // [81]     s-separate-lines(n)       ::=     ( s-l-comments s-flow-line-prefix(n) ) | s-separate-in-line
  val s_separate_lines = P((s_l_comments ~ s_white.rep) | s_separate_in_line)


  // [107]            nb-double-char    ::=     c-ns-esc-char | ( nb-json - “\” - “"” )
  // [108]            ns-double-char    ::=     nb-double-char - s-white
  val nb_double_char = P( c_ns_esc_char | ( !(CharIn("\\-\"")) ~ nb_json  ) )
  val ns_double_char = P( !(s_white) ~ nb_double_char )

  // [109]      c-double-quoted(n,c)    ::=     “"” nb-double-text(n,c) “"”
  // [110]      nb-double-text(n,c)     ::=     c = flow-out  ⇒ nb-double-multi-line(n)
  //                                            c = flow-in   ⇒ nb-double-multi-line(n)
  //                                            c = block-key ⇒ nb-double-one-line
  //                                            c = flow-key  ⇒ nb-double-one-line
  // [111]      nb-double-one-line      ::=     nb-double-char*
  val nb_double_one_line = P( nb_double_char.rep )
  val c_double_quoted = P( "\"" ~/ nb_double_text ~ "\"")

  // [112]      s-double-escaped(n)     ::=     s-white* “\” b-non-content l-empty(n,flow-in)* s-flow-line-prefix(n)
  // [113]      s-double-break(n)       ::=     s-double-escaped(n) | s-flow-folded(n)
  val s_double_escaped = Fail //TODO
  val s_double_break = Fail //TODO

  // [114]      nb-ns-double-in-line    ::=     ( s-white* ns-double-char )*
  // [115]      s-double-next-line(n)   ::=     s-double-break(n) ( ns-double-char nb-ns-double-in-line ( s-double-next-line(n) | s-white* ) )?
  // [116]      nb-double-multi-line(n) ::=     nb-ns-double-in-line ( s-double-next-line(n) | s-white* )
  val nb_ns_double_in_line = P(( s_white.rep ~ ns_double_char).rep)
  val s_double_next_line:Parser[String] = P(s_double_break ~ ( ns_double_char ~ nb_ns_double_in_line ~ ( s_double_next_line | s_white.rep ) ).?)
  val nb_double_multi_line = P(nb_ns_double_in_line ~ ( s_double_next_line | s_white.rep ))

  val nb_double_text = P(Pass.flatMap(unit => this.context match {
    case BlockOut => nb_double_multi_line
    case BlockIn  => nb_double_multi_line
    case FlowOut  => nb_double_one_line
    case FlowIn   => nb_double_one_line
  }))

  // [102]      ns-anchor-char  ::=     ns-char - c-flow-indicator
  // [103]      ns-anchor-name  ::=     ns-anchor-char+
  // [104]      c-ns-alias-node ::=     “*” ns-anchor-name
  val c_ns_alias_node = P("*" ~ (!c_flow_indicator ~ ns_char).rep(1))
  // [105]      e-scalar        ::=     /* Empty */
  val e_scalar = P(Pass)


  // [120]      c-single-quoted(n,c)    ::=     “'” nb-single-text(n,c) “'”
  // [121]      nb-single-text(n,c)     ::=     c = flow-out  ⇒ nb-single-multi-line(n)
  //                                            c = flow-in   ⇒ nb-single-multi-line(n)
  //                                            c = block-key ⇒ nb-single-one-line
  //                                            c = flow-key  ⇒ nb-single-one-line
  // [122]      nb-single-one-line      ::=     nb-single-char*
  val c_single_quoted = Fail //TODO

  // [126]      ns-plain-first(c)       ::=     ( ns-char - c-indicator ) | ( ( “?” | “:” | “-” ) /* Followed by an ns-plain-safe(c)) */ )
  val ns_plain_first = ( !c_indicator ~ ns_char ) | ( CharIn("?:-") ~ &(ns_plain_safe))
  // [127]      ns-plain-safe(c)        ::=     c = flow-out  ⇒ ns-plain-safe-out
  //                                            c = flow-in   ⇒ ns-plain-safe-in
  //                                            c = block-key ⇒ ns-plain-safe-out
  //                                            c = flow-key  ⇒ ns-plain-safe-in         
  // [128]      ns-plain-safe-out       ::=     ns-char
  // [129]      ns-plain-safe-in        ::=     ns-char - c-flow-indicator
  // [130]      ns-plain-char(c)        ::=       ( ns-plain-safe(c) - “:” - “#” )
  //                                            | ( /* An ns-char preceding */ “#” )
  //                                            | ( “:” /* Followed by an ns-plain-safe(c) */ )
  //
  val ns_plain_safe_in = P( !c_flow_indicator ~ ns_char  )
  val ns_plain_safe_out = P( ns_char )
  //val ns_plain_safe_in = CharIn(('\u0021' to '\u007E').diff(",[]{}")) | c_printable_high
  def ns_plain_safe = P(Pass.flatMap(unit => this.context match {
    case FlowOut  => ns_plain_safe_out
    case FlowIn   => ns_plain_safe_in
    case BlockKey => ns_plain_safe_out
    case FlowKey  => ns_plain_safe_in
  }))
  val ns_plain_char = P( (!CharIn(":#") ~ ns_plain_safe) | /*TODO: An ns-char preceding “#” */ ":" ~ &(ns_plain_safe) )

  // val ns_plain_char_flow = ( CharIn(('\u0021' to '\u007E').diff(",[]{}:#")) | c_printable_high
  //                          | (CharIn(",[]{}:#") ~ &("#"))
  //                          | P(":" ~ &(ns_plain_safe_in)) )

  // val ns_plain_char_block = ( CharIn(('\u0021' to '\u007E').diff(":#"))
  //                           | CharIn(":#") ~ &("#")
  //                           | ":" ~ &(ns_char) )



  // [131]          ns-plain(n,c) ::=   c = flow-out  ⇒ ns-plain-multi-line(n,c)
  //                                    c = flow-in   ⇒ ns-plain-multi-line(n,c)
  //                                    c = block-key ⇒ ns-plain-one-line(c)
  //                                    c = flow-key  ⇒ ns-plain-one-line(c)
  // [132] nb-ns-plain-in-line(c) ::=   ( s-white* ns-plain-char(c) )*
  // [133]   ns-plain-one-line(c) ::=   ns-plain-first(c) nb-ns-plain-in-line(c)
  val nb_ns_plain_in_line = P(( s_white.rep ~ ns_plain_char ).rep)
  val ns_plain_one_line = P(ns_plain_first ~ nb_ns_plain_in_line)


  // [134]      s-ns-plain-next-line(n,c)       ::=     s-flow-folded(n) ns-plain-char(c) nb-ns-plain-in-line(c)
  // [135]      ns-plain-multi-line(n,c)        ::=     ns-plain-one-line(c) s-ns-plain-next-line(n,c)*
  val ns_plain_multi_line = Fail // ns-plain-one-line(c) s-ns-plain-next-line(n,c)*

  val ns_plain = P(Pass.flatMap(unit => this.context match {
    case FlowOut  => ns_plain_multi_line
    case FlowIn   => ns_plain_multi_line
    case BlockKey => ns_plain_one_line
    case FlowKey  => ns_plain_one_line
  }))

  // ns_plain_first ( s-white* ns-plain-char(c) )*
  // ( ns-char - c-indicator ) ( s-white* ns-plain-char(c) )* |
  // ( “?” | “:” | “-” ) ( s-white* ns-plain-char(c) )+
  // val ns_plain_one_line_block =
  //val ns_plain_one_line_flow =
  //  ( (CharIn(('\u0021' to '\u007E').diff("-?:,[]{}#&*!|>'\"%@`")) | c_printable_high) ~ (s_white.rep ~ ns_plain_char).rep
   // | (CharIn("?:-") ~ (s_white.rep ~ ns_plain_char).rep(1)) )


  // [137]       c-flow-sequence(n,c)   ::=     “[” s-separate(n,c)? ns-s-flow-seq-entries(n,in-flow(c))? “]”
  val c_flow_sequence = Fail //TODO
  // [140]        c-flow-mapping(n,c)   ::=     “{” s-separate(n,c)? ns-s-flow-map-entries(n,in-flow(c))? “}”
  val c_flow_mapping = Fail //TODO
  // [141] ns-s-flow-map-entries(n,c)   ::=     ns-flow-map-entry(n,c) s-separate(n,c)? ( “,” s-separate(n,c)? ns-s-flow-map-entries(n,c)? )?
  val ns_s_flow_map_entries = Fail //TODO
  // [142]     ns-flow-map-entry(n,c)   ::=     ( “?” s-separate(n,c) ns-flow-map-explicit-entry(n,c) )
  //                                            | ns-flow-map-implicit-entry(n,c)
/*
  val c_flow_mapping = P("{" ~ s_separate_lines.? ~ ns_s_flow_map_entries.? ~ "}").log()
  // ??? support non-scalar keys ???
  // FIXME
  val ns_flow_map_implicit_entry =  ns_plain_one_line_flow ~ ":" ~ !(ns_plain_safe_in) ~ ns_plain_one_line_flow
  val ns_flow_map_explicit_entry = ns_flow_map_implicit_entry //TODO: | map to null 

  val ns_flow_map_entry = P( "?" ~ s_separate_in_line ~ ns_flow_map_explicit_entry ) | ns_flow_map_implicit_entry

  val ns_s_flow_map_entries = (s_separate_in_line.? ~ ns_flow_map_entry.! ~ s_separate_in_line.?).rep(sep=",")
 */

  // [156]      ns-flow-yaml-content(n,c)       ::=     ns-plain(n,c)
  val ns_flow_yaml_content = P(ns_plain)
  // [157]       c-flow-json-content(n,c)       ::=     c-flow-sequence(n,c) | c-flow-mapping(n,c) | c-single-quoted(n,c) | c-double-quoted(n,c)
  val c_flow_json_content = P(c_flow_sequence | c_flow_mapping | c_single_quoted | c_double_quoted)
  // [158]           ns-flow-content(n,c)       ::=     ns-flow-yaml-content(n,c) | c-flow-json-content(n,c)
  val ns_flow_content = P(ns_flow_yaml_content | c_flow_json_content)
  // [161]              ns-flow-node(n,c)       ::=     c-ns-alias-node | ns-flow-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-content(n,c) ) | e-scalar ) )
  val ns_flow_node = P( c_ns_alias_node | ns_flow_content ) //| ( c_ns_properties ~ ( ( s_separate ~ ns_flow_content ) | e_scalar ) )
}


/*
  private abstract class Context {
    val s_line_prefix:Parser[Unit] = Fail
    val s_separate:Parser[Unit]  = s-separate-lines
    val nb_double_text:Parser[String] = Fail.!
    val nb_single_text:Parser[String] = Fail.!
    val ns_plain_safe:Parser[String] = Fail.!
    val ns_plain:Parser[String] = Fail.!
    val in_flow:Context = this
    def seq_spaces:Unit = ()
  }

  private final class BlockOut extends Context {
    override val s_line_prefix:Parser[Unit] =  s_block_line_prefix
    override def seq_spaces = { indentation -= 1 }
  }
  private final class BlockIn extends Context {
    val s_line_prefix:Parser[Unit] = s_block_line_prefix
  }

  private final class FlowOut extends Context {
    override val s_line_prefix:Parser[Unit] = s_flow_line_prefix
    override val nb_double_text:Parser[String] = nb_double_multi_line
    val nb_single_text:Parser[String] = nb_single_multi_line
    val ns_plain_safe:Parser[String] = ns_plain_safe_out
    val ns_plain:Parser[String] = ns_plain_multi_line
    val in_flow:Context = flow_in
  }
  private final class FlowIn extends Context {
    override val s_line_prefix:Parser[Unit] = s_flow_line_prefix
    override val nb_double_text:Parser[String] = nb_double_multi_line
    val nb_single_text:Parser[String] = nb_single_multi_line
    val ns_plain_safe:Parser[String] = ns_plain_safe_in
    val ns_plain:Parser[String] = ns_plain_multi_line
    val in_flow:Context = flow_in
  }

  private final class BlockKey extends Context {
    override val s_separate:Parser[Unit] = s_separate_in_line
    override val nb_double_text:Parser[String] = nb_double_one_line
    val nb_single_text:Parser[String] = nb_single_one_line
    val ns_plain_safe:Parser[String] = ns_plain_safe_out
    val ns_plain:Parser[String] = ns_plain_one_line
    val in_flow:Context = flow_key
  }
  private final class FlowKey extends Context {
    override val s_separate:Parser[Unit] = s_separate_in_line
    override val nb_double_text:Parser[String] = nb_double_one_line
    val nb_single_text:Parser[String] = nb_single_one_line
    val ns_plain_safe:Parser[String] = ns_plain_safe_in
    val ns_plain:Parser[String] = ns_plain_one_line
    val in_flow:Context = flow_key
  }
 */
