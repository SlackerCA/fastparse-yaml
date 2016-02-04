package fastparse.yaml

import fastparse.all._

private class Context
private object BlockOut extends Context
private object BlockIn extends Context
private object FlowOut extends Context
private object FlowIn extends Context
private object BlockKey extends Context
private object FlowKey extends Context


/** 
 * No UTF-32 encodings support.
 * @see http://www.yaml.org/spec/1.2/spec.html
  */
//TODO: move indentation aware parsers to subclass
class YamlParser private (private val indentation:Int, private val context:Context) {
  // [96]       c-ns-properties(n,c)    ::=       ( c-ns-tag-property ( s-separate(n,c) c-ns-anchor-property )? ) | ( c-ns-anchor-property ( s-separate(n,c) c-ns-tag-property )? )
  //TODO: implment node properties
  //val c_ns_properties = P( ( c_ns_tag_property ~ ( s_separate ~ c_ns_anchor_property )? ) | ( c_ns_anchor_property ~ ( s_separate c_ns_tag_property )? ) )

  // [81]     s-separate-lines(n)       ::=     ( s-l-comments s-flow-line-prefix(n) ) | s-separate-in-line
  val s_separate_lines = P((s_l_comments ~ s_white.rep) | s_separate_in_line)


  // [107]            nb-double-char    ::=     c-ns-esc-char | ( nb-json - “\” - “"” )
  // [108]            ns-double-char    ::=     nb-double-char - s-white
  val nb_double_char = P( c_ns_esc_char | ( !(CharIn("\\-\"")) ~ nb_json  ) )
  val ns_double_char = P( !(s_white) ~ nb_double_char.! )

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
  val s_double_break = P("TODO") //TODO

  // [114]      nb-ns-double-in-line    ::=     ( s-white* ns-double-char )*
  // [115]      s-double-next-line(n)   ::=     s-double-break(n) ( ns-double-char nb-ns-double-in-line ( s-double-next-line(n) | s-white* ) )?
  // [116]      nb-double-multi-line(n) ::=     nb-ns-double-in-line ( s-double-next-line(n) | s-white* )
  val nb_ns_double_in_line = P(( s_white.rep ~ ns_double_char).rep)
  val s_double_next_line:Parser[String] = P(s_double_break ~ ( ns_double_char ~ nb_ns_double_in_line ~ ( s_double_next_line | s_white.rep ) ).?.!)
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

object YamlParser extends YamlParser(0,FlowIn) {

//[1]     c-printable                        ::= #x9 | #xA | #xD | [#x20-#x7E] /* 8 bit */ | #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD] /* 16 bit */ | [#x10000-#x10FFFF] /* 32 bit */
  val c_printable = P(CharIn(List('\u0009','\u000A','\u000D','\u0085'), ('\u0020' to '\u007E'), ('\u00A0' to '\uD7FF'), ('\uE000' to '\uFFFD')))
  //[2]     nb-json                            ::= #x9 | [#x20-#x10FFFF]
  val nb_json = P(CharIn(List('\u0009'), '\u0020' to '\uFFFF'))
  //[3]     c-byte-order-mark                  ::= #xFEFF
  val c_byte_order_mark = "\uFEFF"
/*
//[4]     c-sequence-entry                   ::= "-"
val c_sequence_entry = "-"
//[5]     c-mapping-key                      ::= "?"
val c_mapping_key = "?"
//[6]     c-mapping-value                    ::= ":"
val c_mapping_value = ":"
//[7]     c-collect-entry                    ::= ","
val c_collect_entry = ","
//[8]     c-sequence-start                   ::= "["
val c_sequence_start = "["
//[9]     c-sequence-end                     ::= "]"
val c_sequence_end = "]"
//[10]    c-mapping-start                    ::= "{"
val c_mapping_start = "{"
//[11]    c-mapping-end                      ::= "}"
val c_mapping_end = "}"
//[12]    c-comment                          ::= "#"
val c_comment = "#"
//[13]    c-anchor                           ::= "&"
val c_anchor = "&"
//[14]    c-alias                            ::= "*"
val c_alias = "*"
//[15]    c-tag                              ::= "!"
val c_tag = "!"
//[16]    c-literal                          ::= "|"
val c_literal = "|"
//[17]    c-folded                           ::= ">"
val c_folded = ">"
//[18]    c-single-quote                     ::= "'"
val c_single_quote = "'"
//[19]    c-double-quote                     ::= """
val c_double_quote = "\""
//[20]    c-directive                        ::= "%"
val c_directive = "%"
//[21]    c-reserved                         ::= "@" | "`"
val c_reserved = "@" | "`"
*/
  //[22]    c-indicator                        ::= "-" | "?" | ":" | "," | "[" | "]" | "{" | "}" | "#" | "&" | "*" | "!" | "|" | ">" | "'" | """ | "%" | "@" | "`"
  val c_indicator = CharIn("-?:,[]{}#&*!|>'\"%@`")
  //[23]    c-flow-indicator                   ::= "," | "[" | "]" | "{" | "}"
  val c_flow_indicator = ",[]{}"
  //[24]    b-line-feed                        ::= #xA /* LF */
  val b_line_feed = "\u000A"
  //[25]    b-carriage-return                  ::= #xD /* CR */
  val b_carriage_return = "\u000D"
  //[26]    b-char                             ::= b-line-feed | b-carriage-return
  val b_char = b_line_feed | b_carriage_return
  //[27]    nb-char                            ::= c-printable - b-char - c-byte-order-mark
  val nb_char = P(CharIn(List('\u0009', '\u0085'), ('\u00A0' to '\uD7FF'), ('\uE000' to '\uFFFD').diff(c_byte_order_mark)))
  //[28]    b-break                            ::= ( b-carriage-return b-line-feed ) /* DOS, Windows */ | b-carriage-return /* MacOS upto 9.x */ | b-line-feed /* UNIX, MacOS X */
  val b_break = P((b_carriage_return ~ b_line_feed) | b_carriage_return | b_line_feed)
  //[29]    b-as-line-feed                     ::= b-break
  val b_as_line_feed = P(b_break.map(_ => "\u000A"))
  //[30]    b-non-content                      ::= b-break
  val b_non_content = b_break
  //[31]    s-space                            ::= #x20 /* SP */
  val s_space = "\u0020"
  //[32]    s-tab                              ::= #x9 /* TAB */
  val s_tab = "\u0009"
  //[33]    s-white                            ::= s-space | s-tab
  val s_white = CharIn(" \\t")
  //[34]    ns-char                            ::= nb-char - s-white
  val ns_char = P(CharIn(('\u0021' to '\u007E'), ('\u00A0' to '\uD7FF'), ('\uE000' to '\uFFFD').diff(c_byte_order_mark)))
  //[35]    ns-dec-digit                       ::= [#x30-#x39] /* 0-9 */
  val ns_dec_digit = CharIn("0123456789")
  //[36]    ns-hex-digit                       ::= ns-dec-digit | [#x41-#x46] /* A-F */ | [#x61-#x66] /* a-f */
  val ns_hex_digit = CharIn("0123456789ABCDEFabcdef")
  //[37]    ns-ascii-letter                    ::= [#x41-#x5A] /* A-Z */ | [#x61-#x7A] /* a-z */
  val ns_ascii_letter = CharIn(('\u0041' to '\u005A'), ('\u0061' to '\u007A'))
  //[38]    ns-word-char                       ::= ns-dec-digit | ns-ascii-letter | "-"
  val ns_word_char = ns_dec_digit | ns_ascii_letter | "-"
  //[39]    ns-uri-char                        ::= "%" ns-hex-digit ns-hex-digit | ns-word-char | "#" | ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | "," | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")" | "[" | "]"
  val ns_uri_char = P("%" ~ ns_hex_digit ~ ns_hex_digit | ns_word_char | CharIn("#;/?:@&=+$,_.!~*'()[]"))
  //[40]    ns-tag-char                        ::= ns-uri-char - "!" - c-flow-indicator
  val ns_tag_char = P(!(c_flow_indicator+"!") ~ ns_uri_char)
  //[41]    c-escape                           ::= "\"
  val c_escape = "\\"
  //[42]    ns-esc-null                        ::= "0"
  val ns_esc_null = "0"
  //[43]    ns-esc-bell                        ::= "a"
  val ns_esc_bell = "a"
  //[44]    ns-esc-backspace                   ::= "b"
  val ns_esc_backspace = "b"
  //[45]    ns-esc-horizontal-tab              ::= "t" | #x9
  val ns_esc_horizontal_tab = "t" | "\u0009"
  //[46]    ns-esc-line-feed                   ::= "n"
  val ns_esc_line_feed = "n"
  //[47]    ns-esc-vertical-tab                ::= "v"
  val ns_esc_vertical_tab = "v"
  //[48]    ns-esc-form-feed                   ::= "f"
  val ns_esc_form_feed = "f"
  //[49]    ns-esc-carriage-return             ::= "r"
  val ns_esc_carriage_return = "r"
  //[50]    ns-esc-escape                      ::= "e"
  val ns_esc_escape = "e"
  //[51]    ns-esc-space                       ::= #x20
  val ns_esc_space = "\u0020"
  //[52]    ns-esc-double-quote                ::= """
  val ns_esc_double_quote = "\""
  //[53]    ns-esc-slash                       ::= "/"
  val ns_esc_slash = "/"
  //[54]    ns-esc-backslash                   ::= "\"
  val ns_esc_backslash = "\\"
  //[55]    ns-esc-next-line                   ::= "N"
  val ns_esc_next_line = "N"
  //[56]    ns-esc-non-breaking-space          ::= "_"
  val ns_esc_non_breaking_space = "_"
  //[57]    ns-esc-line-separator              ::= "L"
  val ns_esc_line_separator = "L"
  //[58]    ns-esc-paragraph-separator         ::= "P"
  val ns_esc_paragraph_separator = "P"
  //[59]    ns-esc-8-bit                       ::= "x" ( ns-hex-digit × 2 )
  val ns_esc_8_bit = "x" ~ ( ns_hex_digit.rep(min=2,max=2) )
  //[60]    ns-esc-16-bit                      ::= "u" ( ns-hex-digit × 4 )
  val ns_esc_16_bit = "u" ~ ( ns_hex_digit.rep(min=4,max=4) )
  //[61]    ns-esc-32-bit                      ::= "U" ( ns-hex-digit × 8 )
  val ns_esc_32_bit = "U" ~ ( ns_hex_digit.rep(min=8,max=8) )
  //[62]    c-ns-esc-char                      ::= "\" ( ns-esc-null | ns-esc-bell | ns-esc-backspace | ns-esc-horizontal-tab | ns-esc-line-feed | ns-esc-vertical-tab | ns-esc-form-feed | ns-esc-carriage-return | ns-esc-escape | ns-esc-space | ns-esc-double-quote | ns-esc-slash | ns-esc-backslash | ns-esc-next-line | ns-esc-non-breaking-space | ns-esc-line-separator | ns-esc-paragraph-separator | ns-esc-8-bit | ns-esc-16-bit | ns-esc-32-bit )
  //val c_ns_esc_char = "\" ~ ( ns_esc_null | ns_esc_bell | ns_esc_backspace | ns_esc_horizontal_tab | ns_esc_line_feed | ns_esc_vertical_tab | ns_esc_form_feed | ns_esc_carriage_return | ns_esc_escape | ns_esc_space | ns_esc_double_quote | ns_esc_slash | ns_esc_backslash | ns_esc_next_line | ns_esc_non_breaking_space | ns_esc_line_separator | ns_esc_paragraph_separator | ns_esc_8_bit | ns_esc_16_bit | ns_esc_32_bit )
  private val ns_esc_map = Map(
    "\\0" -> '\u0000',
    "\\a" -> '\u0007',
    "\\b" -> '\u0008',
    "\\t" -> '\u0009'
    //TODO: map all escaped chars to values
  )
  private val c_ns_esc_char_escaped = ("\\" ~ CharIn("0abt\tnvfre \"/\\N_LP").!.map(ns_esc_map.get(_)))
  private val c_ns_esc_char_unicode = ("\\x" ~ ns_hex_digit.rep(min=2,max=2).!.map(Integer.parseInt(_, 4)) | "\\u" ~ ns_hex_digit.rep(min=4,max=4).!.map(Integer.parseInt(_, 8)) /*| "\\U" ~ hexDigit(8)*/)
  val c_ns_esc_char = P(c_ns_esc_char_escaped | c_ns_esc_char_unicode)

  //[63]    s-indent(n)                        ::= s-space × n
  val s_indent = P(s_space.rep(indentation))
  //[64]    s-indent(<n)                       ::= s-space × m /* Where m < n */
  val s_indent_lt = P(s_space.rep(min=0,max=indentation-1))
  //[65]    s-indent(≤n)                       ::= s-space × m /* Where m ≤ n */
  val s_indent_le = P(s_space.rep(min=0,max=indentation))

  //[66]    s-separate-in-line                 ::= s-white+ | /* Start of line */
  val s_separate_in_line = s_white.rep(1) //TODO: how to test for Start of line? identify cases where this matters

  //[68]    s-block-line-prefix(n)             ::= s-indent(n)
  val s_block_line_prefix = P(s_indent)
  //[69]    s-flow-line-prefix(n)              ::= s-indent(n) s-separate-in-line?
  val s_flow_line_prefix = P(s_indent) ~ s_separate_in_line.?

  //[67]    s-line-prefix(n,c)                 ::= c = block-out ⇒ s-block-line-prefix(n)
  //                                               c = block-in  ⇒ s-block-line-prefix(n)
  //                                               c = flow-out  ⇒ s-flow-line-prefix(n)
  //                                               c = flow-in   ⇒ s-flow-line-prefix(n)
  val s_line_prefix = P(Pass.flatMap(unit => this.context match {
    case BlockOut => s_block_line_prefix
    case BlockIn  => s_block_line_prefix
    case FlowOut  => s_flow_line_prefix
    case FlowIn   => s_flow_line_prefix
  }))

  //[70]    l-empty(n,c)                       ::= ( s-line-prefix(n,c) | s-indent(<n) ) b-as-line-feed
  val l_empty = ( s_line_prefix | s_indent_lt ) ~ b_as_line_feed
  //[71]    b-l-trimmed(n,c)                   ::= b-non-content l-empty(n,c)+
  val b_l_trimmed = b_non_content ~ l_empty.rep(1)
  //[72]    b-as-space                         ::= b-break
  val b_as_space = b_break
  //[73]    b-l-folded(n,c)                    ::= b-l-trimmed(n,c) | b-as-space
  val b_l_folded = b_l_trimmed | b_as_space
  //[74]    s-flow-folded(n)                   ::= s-separate-in-line? b-l-folded(n,flow-in) s-flow-line-prefix(n)
  val s_flow_folded = s_separate_in_line.? ~ Pass.flatMap(YamlParser(indentation,FlowIn).b_l_folded) ~ s_flow_line_prefix(n)

  //[75]    c-nb-comment-text                  ::= "#" nb-char*
  val c_nb_comment_text = "#" ~ nb_char.rep
  //[76]    b-comment                          ::= b-non-content | /* End of file */
  val b_comment = b_non_content | End
  //[77]    s-b-comment                        ::= ( s-separate-in-line c-nb-comment-text? )? b-comment
  val s_b_comment = ( s_separate_in_line ~ c_nb_comment_text.? ).? ~ b_comment
  //[78]    l-comment                          ::= s-separate-in-line c-nb-comment-text? b-comment
  val l_comment = s_separate_in_line ~ c_nb_comment_text.? ~ b_comment
  //[79]    s-l-comments                       ::= ( s-b-comment | /* Start of line */ ) l-comment*
  val s_l_comments = ( s_b_comment /*|  Start of line */ ) ~ l_comment.rep //TODO: how to test for Start of line? identify cases where this matters

  //[80]    s-separate(n,c)                    ::= c = block-out ⇒ s-separate-lines(n)
  //                                               c = block-in  ⇒ s-separate-lines(n)
  //                                               c = flow-out  ⇒ s-separate-lines(n)
  //                                               c = flow-in   ⇒ s-separate-lines(n)
  //                                               c = block-key ⇒ s-separate-in-line
  //                                               c = flow-key  ⇒ s-separate-in-line
  val s_separate = P(Pass.flatMap(unit => this.context match {
    case BlockOut => s-separate-lines
    case BlockIn  => s-separate-lines
    case FlowOut  => s-separate-lines
    case FlowIn   => s-separate-lines
    case BlockKey => s-separate-in-line
    case FlowKey  => s-separate-in-line
  }))
  //[81]    s-separate-lines(n)                ::= ( s-l-comments s-flow-line-prefix(n) ) | s-separate-in-line
  val s_separate_lines(n) = ( s_l_comments ~ s_flow_line_prefix(n) ) | s_separate_in_line

  //[82]    l-directive                        ::= "%" ( ns-yaml-directive | ns-tag-directive | ns-reserved-directive ) s-l-comments
  val l_directive = "%" ~ ( ns_yaml_directive | ns_tag_directive | ns_reserved_directive ) ~ s_l_comments
  //[83]    ns-reserved-directive              ::= ns-directive-name ( s-separate-in-line ns-directive-parameter )*
  val ns_reserved_directive = ns_directive_name ~ ( s_separate_in_line ~ ns_directive_parameter ).rep
  //[84]    ns-directive-name                  ::= ns-char+
  val ns_directive_name = ns_char.rep(1)
  //[85]    ns-directive-parameter             ::= ns-char+
  val ns_directive_parameter = ns_char.rep(1)
  //[86]    ns-yaml-directive                  ::= "Y" "A" "M" "L" s-separate-in-line ns-yaml-version
  val ns_yaml_directive = "YAML" ~ s_separate_in_line ~ ns_yaml_version
  //[87]    ns-yaml-version                    ::= ns-dec-digit+ "." ns-dec-digit+
  val ns_yaml_version = ns_dec_digit.rep(1) ~ "." ~ ns_dec_digit.rep(1)
  //[88]    ns-tag-directive                   ::= "T" "A" "G" s-separate-in-line c-tag-handle s-separate-in-line ns-tag-prefix
  val ns_tag_directive = "TAG" ~ s_separate_in_line ~ c_tag_handle ~ s_separate_in_line ~ ns_tag_prefix
  //[89]    c-tag-handle                       ::= c-named-tag-handle | c-secondary-tag-handle | c-primary-tag-handle
  val c_tag_handle = c_named_tag_handle | c_secondary_tag_handle | c_primary_tag_handle
  //[90]    c-primary-tag-handle               ::= "!"
  val c_primary_tag_handle = "!"
  //[91]    c-secondary-tag-handle             ::= "!" "!"
  val c_secondary_tag_handle = "!!"
  //[92]    c-named-tag-handle                 ::= "!" ns-word-char+ "!"
  val c_named_tag_handle = "!" ~ ns_word_char.rep(1) ~ "!"
  //[93]    ns-tag-prefix                      ::= c-ns-local-tag-prefix | ns-global-tag-prefix
  val ns_tag_prefix = c_ns_local_tag_prefix | ns_global_tag_prefix
  //[94]    c-ns-local-tag-prefix              ::= "!" ns-uri-char*
  val c_ns_local_tag_prefix = "!" ~ ns_uri_char.rep
  //[95]    ns-global-tag-prefix               ::= ns-tag-char ns-uri-char*
  val ns_global_tag_prefix = ns_tag_char ~ ns_uri_char.rep

  //[96]    c-ns-properties(n,c)               ::= ( c-ns-tag-property ( s-separate(n,c) c-ns-anchor-property )? ) | ( c-ns-anchor-property ( s-separate(n,c) c-ns-tag-property )? )
  val c_ns_properties = ( c_ns_tag_property ~ ( s_separate ~ c_ns_anchor_property ).? ) | ( c_ns_anchor_property ~ ( s_separate ~ c_ns_tag_property ).? )

//[97]    c-ns-tag-property                  ::= c-verbatim-tag | c-ns-shorthand-tag | c-non-specific-tag
val c_ns_tag_property = c_verbatim_tag | c_ns_shorthand_tag | c_non_specific_tag
//[98]    c-verbatim-tag                     ::= "!" "<" ns-uri-char+ ">"
val c_verbatim_tag = "!<" ~ ns_uri_char.rep(1) ~ ">"
//[99]    c-ns-shorthand-tag                 ::= c-tag-handle ns-tag-char+
val c_ns_shorthand_tag = c_tag_handle ~ ns_tag_char.rep(1)
//[100]   c-non-specific-tag                 ::= "!"
val c_non_specific_tag = "!"
//[101]   c-ns-anchor-property               ::= "&" ns-anchor-name
val c_ns_anchor_property = "&" ~ ns_anchor_name
//[102]   ns-anchor-char                     ::= ns-char - c-flow-indicator
val ns_anchor_char = ns_char - c_flow_indicator
//[103]   ns-anchor-name                     ::= ns-anchor-char+
val ns_anchor_name = ns_anchor_char.rep(1)
//[104]   c-ns-alias-node                    ::= "*" ns-anchor-name
val c_ns_alias_node = "*" ~ ns_anchor_name
//[105]   e-scalar                           ::= /* Empty */
val e_scalar = /* Empty */
//[106]   e-node                             ::= e-scalar
val e_node = e_scalar
//[107]   nb-double-char                     ::= c-ns-esc-char | ( nb-json - "\" - """ )
val nb_double_char = c_ns_esc_char | ( nb_json - "\" - "\"" )
//[108]   ns-double-char                     ::= nb-double-char - s-white
val ns_double_char = nb_double_char - s_white

  //[109]   c-double-quoted(n,c)               ::= """ nb-double-text(n,c) """
val c_double_quoted(n,c) = "\"" ~ nb_double_text(n,c) ~ "\""
//[110]   nb-double-text(n,c)                ::= c = flow-out ⇒ nb-double-multi-line(n)
val nb_double_text(n,c) = c = flow_out ⇒ nb_double_multi_line(n)
//                                               c = flow-in   ⇒ nb-double-multi-line(n)
//                                               c = block-key ⇒ nb-double-one-line
//                                               c = flow-key  ⇒ nb-double-one-line
//[111]   nb-double-one-line                 ::= nb-double-char*
val nb_double_one_line = nb_double_char.rep
//[112]   s-double-escaped(n)                ::= s-white* "\" b-non-content l-empty(n,flow-in)* s-flow-line-prefix(n)
val s_double_escaped(n) = s_white.rep ~ "\" ~ b_non_content ~ l_empty(n,flow_in).rep ~ s_flow_line_prefix(n)
//[113]   s-double-break(n)                  ::= s-double-escaped(n) | s-flow-folded(n)
val s_double_break(n) = s_double_escaped(n) | s_flow_folded(n)
//[114]   nb-ns-double-in-line               ::= ( s-white* ns-double-char )*
val nb_ns_double_in_line = ( s_white.rep ~ ns_double_char ).rep
//[115]   s-double-next-line(n)              ::= s-double-break(n) ( ns-double-char nb-ns-double-in-line ( s-double-next-line(n) | s-white* ) )?
val s_double_next_line(n) = s_double_break(n) ~ ( ns_double_char ~ nb_ns_double_in_line ~ ( s_double_next_line(n) | s_white.rep ) ).?
//[116]   nb-double-multi-line(n)            ::= nb-ns-double-in-line ( s-double-next-line(n) | s-white* )
val nb_double_multi_line(n) = nb_ns_double_in_line ~ ( s_double_next_line(n) | s_white.rep )

  //[117]   c-quoted-quote                     ::= "'" "'"
val c_quoted_quote = "''"
//[118]   nb-single-char                     ::= c-quoted-quote | ( nb-json - "'" )
val nb_single_char = c_quoted_quote | ( nb_json - "'" )
//[119]   ns-single-char                     ::= nb-single-char - s-white
val ns_single_char = nb_single_char - s_white

  //[120]   c-single-quoted(n,c)               ::= "'" nb-single-text(n,c) "'"
val c_single_quoted(n,c) = "'" ~ nb_single_text(n,c) ~ "'"
//[121]   nb-single-text(n,c)                ::= c = flow-out ⇒ nb-single-multi-line(n)
val nb_single_text(n,c) = c = flow_out ⇒ nb_single_multi_line(n)
//                                               c = flow-in   ⇒ nb-single-multi-line(n)
//                                               c = block-key ⇒ nb-single-one-line
//                                               c = flow-key  ⇒ nb-single-one-line

  //[122]   nb-single-one-line                 ::= nb-single-char*
val nb_single_one_line = nb_single_char.rep
//[123]   nb-ns-single-in-line               ::= ( s-white* ns-single-char )*
val nb_ns_single_in_line = ( s_white.rep ~ ns_single_char ).rep

  //[124]   s-single-next-line(n)              ::= s-flow-folded(n) ( ns-single-char nb-ns-single-in-line ( s-single-next-line(n) | s-white* ) )?
val s_single_next_line(n) = s_flow_folded(n) ~ ( ns_single_char ~ nb_ns_single_in_line ~ ( s_single_next_line(n) | s_white.rep ) ).?
//[125]   nb-single-multi-line(n)            ::= nb-ns-single-in-line ( s-single-next-line(n) | s-white* )
val nb_single_multi_line(n) = nb_ns_single_in_line ~ ( s_single_next_line(n) | s_white.rep )
//[126]   ns-plain-first(c)                  ::= ( ns-char - c-indicator ) | ( ( "?" | ":" | "-" ) /* Followed by an ns-plain-safe(c)) */ )
val ns_plain_first(c) = ( ns_char - c_indicator ) | ( CharIn( "?:-" ) /* Followed by an ns_plain_safe(c)) */ )
//[127]   ns-plain-safe(c)                   ::= c = flow-out ⇒ ns-plain-safe-out
val ns_plain_safe(c) = c = flow_out ⇒ ns_plain_safe_out
//                                               c = flow-in   ⇒ ns-plain-safe-in
//                                               c = block-key ⇒ ns-plain-safe-out
//                                               c = flow-key  ⇒ ns-plain-safe-in

  //[128]   ns-plain-safe-out                  ::= ns-char
val ns_plain_safe_out = ns_char
//[129]   ns-plain-safe-in                   ::= ns-char - c-flow-indicator
val ns_plain_safe_in = ns_char - c_flow_indicator

  //[130]   ns-plain-char(c)                   ::= ( ns-plain-safe(c) - ":" - "#" ) | ( /* An ns-char preceding */ "#" ) | ( ":" /* Followed by an ns-plain-safe(c) */ )
val ns_plain_char(c) = ( ns_plain_safe(c) - ":" - "#" ) | ( /* An ns_char ~ preceding */ "#" ) | ( ":" /* Followed by an ns_plain_safe(c) */ )
//[131]   ns-plain(n,c)                      ::= c = flow-out ⇒ ns-plain-multi-line(n,c)
val ns_plain(n,c) = c = flow_out ⇒ ns_plain_multi_line(n,c)
//                                               c = flow-in   ⇒ ns-plain-multi-line(n,c)
//                                               c = block-key ⇒ ns-plain-one-line(c)
//                                               c = flow-key  ⇒ ns-plain-one-line(c)
//[132]   nb-ns-plain-in-line(c)             ::= ( s-white* ns-plain-char(c) )*
val nb_ns_plain_in_line(c) = ( s_white.rep ~ ns_plain_char(c) ).rep
//[133]   ns-plain-one-line(c)               ::= ns-plain-first(c) nb-ns-plain-in-line(c)
val ns_plain_one_line(c) = ns_plain_first(c) ~ nb_ns_plain_in_line(c)
//[134]   s-ns-plain-next-line(n,c)          ::= s-flow-folded(n) ns-plain-char(c) nb-ns-plain-in-line(c)
val s_ns_plain_next_line(n,c) = s_flow_folded(n) ~ ns_plain_char(c) ~ nb_ns_plain_in_line(c)
//[135]   ns-plain-multi-line(n,c)           ::= ns-plain-one-line(c) s-ns-plain-next-line(n,c)*
val ns_plain_multi_line(n,c) = ns_plain_one_line(c) ~ s_ns_plain_next_line(n,c).rep
//[136]   in-flow(c)                         ::= c = flow-out ⇒ flow-in
val in_flow(c) = c = flow_out ⇒ flow_in
//                                               c = flow-in   ⇒ flow-in
//                                               c = block-key ⇒ flow-key
//                                               c = flow-key  ⇒ flow-key
//[137]   c-flow-sequence(n,c)               ::= "[" s-separate(n,c)? ns-s-flow-seq-entries(n,in-flow(c))? "]"
val c_flow_sequence(n,c) = "[" ~ s_separate(n,c).? ~ ns_s_flow_seq_entries(n,in_flow(c)).? ~ "]"
//[138]   ns-s-flow-seq-entries(n,c)         ::= ns-flow-seq-entry(n,c) s-separate(n,c)? ( "," s-separate(n,c)? ns-s-flow-seq-entries(n,c)? )?
val ns_s_flow_seq_entries(n,c) = ns_flow_seq_entry(n,c) ~ s_separate(n,c).? ~ ( "," ~ s_separate(n,c).? ~ ns_s_flow_seq_entries(n,c).? ).?
//[139]   ns-flow-seq-entry(n,c)             ::= ns-flow-pair(n,c) | ns-flow-node(n,c)
val ns_flow_seq_entry(n,c) = ns_flow_pair(n,c) | ns_flow_node(n,c)
//[140]   c-flow-mapping(n,c)                ::= "{" s-separate(n,c)? ns-s-flow-map-entries(n,in-flow(c))? "}"
val c_flow_mapping(n,c) = "{" ~ s_separate(n,c).? ~ ns_s_flow_map_entries(n,in_flow(c)).? ~ "}"
//[141]   ns-s-flow-map-entries(n,c)         ::= ns-flow-map-entry(n,c) s-separate(n,c)? ( "," s-separate(n,c)? ns-s-flow-map-entries(n,c)? )?
val ns_s_flow_map_entries(n,c) = ns_flow_map_entry(n,c) ~ s_separate(n,c).? ~ ( "," ~ s_separate(n,c).? ~ ns_s_flow_map_entries(n,c).? ).?
//[142]   ns-flow-map-entry(n,c)             ::= ( "?" s-separate(n,c) ns-flow-map-explicit-entry(n,c) ) | ns-flow-map-implicit-entry(n,c)
val ns_flow_map_entry(n,c) = ( "?" ~ s_separate(n,c) ~ ns_flow_map_explicit_entry(n,c) ) | ns_flow_map_implicit_entry(n,c)
//[143]   ns-flow-map-explicit-entry(n,c)    ::= ns-flow-map-implicit-entry(n,c) | ( e-node /* Key */ e-node /* Value */ )
val ns_flow_map_explicit_entry(n,c) = ns_flow_map_implicit_entry(n,c) | ( e_node /* Key */ e_node /* Value */ )
//[144]   ns-flow-map-implicit-entry(n,c)    ::= ns-flow-map-yaml-key-entry(n,c) | c-ns-flow-map-empty-key-entry(n,c) | c-ns-flow-map-json-key-entry(n,c)
val ns_flow_map_implicit_entry(n,c) = ns_flow_map_yaml_key_entry(n,c) | c_ns_flow_map_empty_key_entry(n,c) | c_ns_flow_map_json_key_entry(n,c)
//[145]   ns-flow-map-yaml-key-entry(n,c)    ::= ns-flow-yaml-node(n,c) ( ( s-separate(n,c)? c-ns-flow-map-separate-value(n,c) ) | e-node )
val ns_flow_map_yaml_key_entry(n,c) = ns_flow_yaml_node(n,c) ~ ( ( s_separate(n,c).? ~ c_ns_flow_map_separate_value(n,c) ) | e_node )
//[146]   c-ns-flow-map-empty-key-entry(n,c) ::= e-node /* Key */ c-ns-flow-map-separate-value(n,c)
val c_ns_flow_map_empty_key_entry(n,c) = e_node /* Key */ c_ns_flow_map_separate_value(n,c)
//[147]   c-ns-flow-map-separate-value(n,c)  ::= ":" /* Not followed by an ns-plain-safe(c) */ ( ( s-separate(n,c) ns-flow-node(n,c) ) | e-node /* Value */ )
val c_ns_flow_map_separate_value(n,c) = ":" /* Not followed by an ns_plain_safe(c) */ ( ( s_separate(n,c) ~ ns_flow_node(n,c) ) | e_node /* Value */ )
//[148]   c-ns-flow-map-json-key-entry(n,c)  ::= c-flow-json-node(n,c) ( ( s-separate(n,c)? c-ns-flow-map-adjacent-value(n,c) ) | e-node )
val c_ns_flow_map_json_key_entry(n,c) = c_flow_json_node(n,c) ~ ( ( s_separate(n,c).? ~ c_ns_flow_map_adjacent_value(n,c) ) | e_node )
//[149]   c-ns-flow-map-adjacent-value(n,c)  ::= ":" ( ( s-separate(n,c)? ns-flow-node(n,c) ) | e-node ) /* Value */
val c_ns_flow_map_adjacent_value(n,c) = ":" ~ ( ( s_separate(n,c).? ~ ns_flow_node(n,c) ) | e_node ) /* Value */
//[150]   ns-flow-pair(n,c)                  ::= ( "?" s-separate(n,c) ns-flow-map-explicit-entry(n,c) ) | ns-flow-pair-entry(n,c)
val ns_flow_pair(n,c) = ( "?" ~ s_separate(n,c) ~ ns_flow_map_explicit_entry(n,c) ) | ns_flow_pair_entry(n,c)
//[151]   ns-flow-pair-entry(n,c)            ::= ns-flow-pair-yaml-key-entry(n,c) | c-ns-flow-map-empty-key-entry(n,c) | c-ns-flow-pair-json-key-entry(n,c)
val ns_flow_pair_entry(n,c) = ns_flow_pair_yaml_key_entry(n,c) | c_ns_flow_map_empty_key_entry(n,c) | c_ns_flow_pair_json_key_entry(n,c)
//[152]   ns-flow-pair-yaml-key-entry(n,c)   ::= ns-s-implicit-yaml-key(flow-key) c-ns-flow-map-separate-value(n,c)
val ns_flow_pair_yaml_key_entry(n,c) = ns_s_implicit_yaml_key(flow_key) ~ c_ns_flow_map_separate_value(n,c)
//[153]   c-ns-flow-pair-json-key-entry(n,c) ::= c-s-implicit-json-key(flow-key) c-ns-flow-map-adjacent-value(n,c)
val c_ns_flow_pair_json_key_entry(n,c) = c_s_implicit_json_key(flow_key) ~ c_ns_flow_map_adjacent_value(n,c)
//[154]   ns-s-implicit-yaml-key(c)          ::= ns-flow-yaml-node(n/a,c) s-separate-in-line? /* At most 1024 characters altogether */
val ns_s_implicit_yaml_key(c) = ns_flow_yaml_node(n/a,c) ~ s_separate_in_line.? /* At most 1024 characters altogether */
//[155]   c-s-implicit-json-key(c)           ::= c-flow-json-node(n/a,c) s-separate-in-line? /* At most 1024 characters altogether */
val c_s_implicit_json_key(c) = c_flow_json_node(n/a,c) ~ s_separate_in_line.? /* At most 1024 characters altogether */
//[156]   ns-flow-yaml-content(n,c)          ::= ns-plain(n,c)
val ns_flow_yaml_content(n,c) = ns_plain(n,c)
//[157]   c-flow-json-content(n,c)           ::= c-flow-sequence(n,c) | c-flow-mapping(n,c) | c-single-quoted(n,c) | c-double-quoted(n,c)
val c_flow_json_content(n,c) = c_flow_sequence(n,c) | c_flow_mapping(n,c) | c_single_quoted(n,c) | c_double_quoted(n,c)
//[158]   ns-flow-content(n,c)               ::= ns-flow-yaml-content(n,c) | c-flow-json-content(n,c)
val ns_flow_content(n,c) = ns_flow_yaml_content(n,c) | c_flow_json_content(n,c)
//[159]   ns-flow-yaml-node(n,c)             ::= c-ns-alias-node | ns-flow-yaml-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-yaml-content(n,c) ) | e-scalar ) )
val ns_flow_yaml_node(n,c) = c_ns_alias_node | ns_flow_yaml_content(n,c) | ( c_ns_properties(n,c) ~ ( ( s_separate(n,c) ~ ns_flow_yaml_content(n,c) ) | e_scalar ) )
//[160]   c-flow-json-node(n,c)              ::= ( c-ns-properties(n,c) s-separate(n,c) )? c-flow-json-content(n,c)
val c_flow_json_node(n,c) = ( c_ns_properties(n,c) ~ s_separate(n,c) ).? ~ c_flow_json_content(n,c)
//[161]   ns-flow-node(n,c)                  ::= c-ns-alias-node | ns-flow-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-content(n,c) ) | e-scalar ) )
val ns_flow_node(n,c) = c_ns_alias_node | ns_flow_content(n,c) | ( c_ns_properties(n,c) ~ ( ( s_separate(n,c) ~ ns_flow_content(n,c) ) | e_scalar ) )
//[162]   c-b-block-header(m,t)              ::= ( ( c-indentation-indicator(m) c-chomping-indicator(t) ) | ( c-chomping-indicator(t) c-indentation-indicator(m) ) ) s-b-comment
val c_b_block_header(m,t) = ( ( c_indentation_indicator(m) ~ c_chomping_indicator(t) ) | ( c_chomping_indicator(t) ~ c_indentation_indicator(m) ) ) ~ s_b_comment
//[163]   c-indentation-indicator(m)         ::= ns-dec-digit ⇒ m = ns-dec-digit - #x30
val c_indentation_indicator(m) = ns_dec_digit ⇒ m = ns_dec_digit - \u0030
//                                               /* Empty */  ⇒ m = auto-detect()
//[164]   c-chomping-indicator(t)            ::= "-" ⇒ t = strip
val c_chomping_indicator(t) = "-" ⇒ t = strip
//                                               "+"         ⇒ t = keep
//                                               /* Empty */ ⇒ t = clip
//[165]   b-chomped-last(t)                  ::= t = strip ⇒ b-non-content | /* End of file */
val b_chomped_last(t) = t = strip ⇒ b_non_content | End
//                                               t = clip  ⇒ b-as-line-feed | /* End of file */
//                                               t = keep  ⇒ b-as-line-feed | /* End of file */
//[166]   l-chomped-empty(n,t)               ::= t = strip ⇒ l-strip-empty(n)
val l_chomped_empty(n,t) = t = strip ⇒ l_strip_empty(n)
//                                               t = clip  ⇒ l-strip-empty(n)
//                                               t = keep  ⇒ l-keep-empty(n)
//[167]   l-strip-empty(n)                   ::= ( s-indent(≤n) b-non-content )* l-trail-comments(n)?
val l_strip_empty(n) = ( s_indent(≤n) ~ b_non_content ).rep ~ l_trail_comments(n).?
//[168]   l-keep-empty(n)                    ::= l-empty(n,block-in)* l-trail-comments(n)?
val l_keep_empty(n) = l_empty(n,block_in).rep ~ l_trail_comments(n).?
//[169]   l-trail-comments(n)                ::= s-indent(<n) c-nb-comment-text b-comment l-comment*
val l_trail_comments(n) = s_indent(<n) ~ c_nb_comment_text ~ b_comment ~ l_comment.rep
//[170]   c-l+literal(n)                     ::= "|" c-b-block-header(m,t) l-literal-content(n+m,t)
val c_l+literal(n) = "|" ~ c_b_block_header(m,t) ~ l_literal_content(n+m,t)
//[171]   l-nb-literal-text(n)               ::= l-empty(n,block-in)* s-indent(n) nb-char+
val l_nb_literal_text(n) = l_empty(n,block_in).rep ~ s_indent(n) ~ nb_char.rep(1)
//[172]   b-nb-literal-next(n)               ::= b-as-line-feed l-nb-literal-text(n)
val b_nb_literal_next(n) = b_as_line_feed ~ l_nb_literal_text(n)
//[173]   l-literal-content(n,t)             ::= ( l-nb-literal-text(n) b-nb-literal-next(n)* b-chomped-last(t) )? l-chomped-empty(n,t)
val l_literal_content(n,t) = ( l_nb_literal_text(n) ~ b_nb_literal_next(n).rep ~ b_chomped_last(t) ).? ~ l_chomped_empty(n,t)
//[174]   c-l+folded(n)                      ::= ">" c-b-block-header(m,t) l-folded-content(n+m,t)
val c_l+folded(n) = ">" ~ c_b_block_header(m,t) ~ l_folded_content(n+m,t)
//[175]   s-nb-folded-text(n)                ::= s-indent(n) ns-char nb-char*
val s_nb_folded_text(n) = s_indent(n) ~ ns_char ~ nb_char.rep
//[176]   l-nb-folded-lines(n)               ::= s-nb-folded-text(n) ( b-l-folded(n,block-in) s-nb-folded-text(n) )*
val l_nb_folded_lines(n) = s_nb_folded_text(n) ~ ( b_l_folded(n,block_in) ~ s_nb_folded_text(n) ).rep
//[177]   s-nb-spaced-text(n)                ::= s-indent(n) s-white nb-char*
val s_nb_spaced_text(n) = s_indent(n) ~ s_white ~ nb_char.rep
//[178]   b-l-spaced(n)                      ::= b-as-line-feed l-empty(n,block-in)*
val b_l_spaced(n) = b_as_line_feed ~ l_empty(n,block_in).rep
//[179]   l-nb-spaced-lines(n)               ::= s-nb-spaced-text(n) ( b-l-spaced(n) s-nb-spaced-text(n) )*
val l_nb_spaced_lines(n) = s_nb_spaced_text(n) ~ ( b_l_spaced(n) ~ s_nb_spaced_text(n) ).rep
//[180]   l-nb-same-lines(n)                 ::= l-empty(n,block-in)* ( l-nb-folded-lines(n) | l-nb-spaced-lines(n) )
val l_nb_same_lines(n) = l_empty(n,block_in).rep ~ ( l_nb_folded_lines(n) | l_nb_spaced_lines(n) )
//[181]   l-nb-diff-lines(n)                 ::= l-nb-same-lines(n) ( b-as-line-feed l-nb-same-lines(n) )*
val l_nb_diff_lines(n) = l_nb_same_lines(n) ~ ( b_as_line_feed ~ l_nb_same_lines(n) ).rep
//[182]   l-folded-content(n,t)              ::= ( l-nb-diff-lines(n) b-chomped-last(t) )? l-chomped-empty(n,t)
val l_folded_content(n,t) = ( l_nb_diff_lines(n) ~ b_chomped_last(t) ).? ~ l_chomped_empty(n,t)
//[183]   l+block-sequence(n)                ::= ( s-indent(n+m) c-l-block-seq-entry(n+m) )+ /* For some fixed auto-detected m > 0 */
val l+block_sequence(n) = ( s_indent(n+m) ~ c_l_block_seq_entry(n+m) ).rep(1) /* For some fixed auto_detected ~ m > 0 */
//[184]   c-l-block-seq-entry(n)             ::= "-" /* Not followed by an ns-char */ s-l+block-indented(n,block-in)
val c_l_block_seq_entry(n) = "-" /* Not followed by an ns_char */ s_l+block_indented(n,block_in)
//[185]   s-l+block-indented(n,c)            ::= ( s-indent(m) ( ns-l-compact-sequence(n+1+m) | ns-l-compact-mapping(n+1+m) ) ) | s-l+block-node(n,c) | ( e-node s-l-comments )
val s_l+block_indented(n,c) = ( s_indent(m) ~ ( ns_l_compact_sequence(n+1+m) | ns_l_compact_mapping(n+1+m) ) ) | s_l+block_node(n,c) | ( e_node ~ s_l_comments )
//[186]   ns-l-compact-sequence(n)           ::= c-l-block-seq-entry(n) ( s-indent(n) c-l-block-seq-entry(n) )*
val ns_l_compact_sequence(n) = c_l_block_seq_entry(n) ~ ( s_indent(n) ~ c_l_block_seq_entry(n) ).rep
//[187]   l+block-mapping(n)                 ::= ( s-indent(n+m) ns-l-block-map-entry(n+m) )+ /* For some fixed auto-detected m > 0 */
val l+block_mapping(n) = ( s_indent(n+m) ~ ns_l_block_map_entry(n+m) ).rep(1) /* For some fixed auto_detected ~ m > 0 */
//[188]   ns-l-block-map-entry(n)            ::= c-l-block-map-explicit-entry(n) | ns-l-block-map-implicit-entry(n)
val ns_l_block_map_entry(n) = c_l_block_map_explicit_entry(n) | ns_l_block_map_implicit_entry(n)
//[189]   c-l-block-map-explicit-entry(n)    ::= c-l-block-map-explicit-key(n) ( l-block-map-explicit-value(n) | e-node )
val c_l_block_map_explicit_entry(n) = c_l_block_map_explicit_key(n) ~ ( l_block_map_explicit_value(n) | e_node )
//[190]   c-l-block-map-explicit-key(n)      ::= "?" s-l+block-indented(n,block-out)
val c_l_block_map_explicit_key(n) = "?" ~ s_l+block_indented(n,block_out)
//[191]   l-block-map-explicit-value(n)      ::= s-indent(n) ":" s-l+block-indented(n,block-out)
val l_block_map_explicit_value(n) = s_indent(n) ~ ":" ~ s_l+block_indented(n,block_out)
//[192]   ns-l-block-map-implicit-entry(n)   ::= ( ns-s-block-map-implicit-key | e-node ) c-l-block-map-implicit-value(n)
val ns_l_block_map_implicit_entry(n) = ( ns_s_block_map_implicit_key | e_node ) ~ c_l_block_map_implicit_value(n)
//[193]   ns-s-block-map-implicit-key        ::= c-s-implicit-json-key(block-key) | ns-s-implicit-yaml-key(block-key)
val ns_s_block_map_implicit_key = c_s_implicit_json_key(block_key) | ns_s_implicit_yaml_key(block_key)
//[194]   c-l-block-map-implicit-value(n)    ::= ":" ( s-l+block-node(n,block-out) | ( e-node s-l-comments ) )
val c_l_block_map_implicit_value(n) = ":" ~ ( s_l+block_node(n,block_out) | ( e_node ~ s_l_comments ) )
//[195]   ns-l-compact-mapping(n)            ::= ns-l-block-map-entry(n) ( s-indent(n) ns-l-block-map-entry(n) )*
val ns_l_compact_mapping(n) = ns_l_block_map_entry(n) ~ ( s_indent(n) ~ ns_l_block_map_entry(n) ).rep
//[196]   s-l+block-node(n,c)                ::= s-l+block-in-block(n,c) | s-l+flow-in-block(n)
val s_l+block_node(n,c) = s_l+block_in_block(n,c) | s_l+flow_in_block(n)
//[197]   s-l+flow-in-block(n)               ::= s-separate(n+1,flow-out) ns-flow-node(n+1,flow-out) s-l-comments
val s_l+flow_in_block(n) = s_separate(n+1,flow_out) ~ ns_flow_node(n+1,flow_out) ~ s_l_comments
//[198]   s-l+block-in-block(n,c)            ::= s-l+block-scalar(n,c) | s-l+block-collection(n,c)
val s_l+block_in_block(n,c) = s_l+block_scalar(n,c) | s_l+block_collection(n,c)
//[199]   s-l+block-scalar(n,c)              ::= s-separate(n+1,c) ( c-ns-properties(n+1,c) s-separate(n+1,c) )? ( c-l+literal(n) | c-l+folded(n) )
val s_l+block_scalar(n,c) = s_separate(n+1,c) ~ ( c_ns_properties(n+1,c) ~ s_separate(n+1,c) ).? ~ ( c_l+literal(n) | c_l+folded(n) )
//[200]   s-l+block-collection(n,c)          ::= ( s-separate(n+1,c) c-ns-properties(n+1,c) )? s-l-comments ( l+block-sequence(seq-spaces(n,c)) | l+block-mapping(n) )
val s_l+block_collection(n,c) = ( s_separate(n+1,c) ~ c_ns_properties(n+1,c) ).? ~ s_l_comments ~ ( l+block_sequence(seq_spaces(n,c)) | l+block_mapping(n) )
//[201]   seq-spaces(n,c)                    ::= c = block-out ⇒ n-1
val seq_spaces(n,c) = c = block_out ⇒ n_1

  //                                               c = block-in  ⇒ n
//[202]   l-document-prefix                  ::= c-byte-order-mark? l-comment*
val l_document_prefix = c_byte_order_mark.? ~ l_comment.rep
//[203]   c-directives-end                   ::= "-" "-" "-"
val c_directives_end = "---"
//[204]   c-document-end                     ::= "." "." "."
val c_document_end = "..."
//[205]   l-document-suffix                  ::= c-document-end s-l-comments
val l_document_suffix = c_document_end ~ s_l_comments
//[206]   c-forbidden                        ::= /* Start of line */ ( c-directives-end | c-document-end ) ( b-char | s-white | /* End of file */ )
val c_forbidden = /* Start of line */ ( c_directives_end | c_document_end ) ~ ( b_char | s_white | End )
//[207]   l-bare-document                    ::= s-l+block-node(-1,block-in) /* Excluding c-forbidden content */
val l_bare_document = s_l+block_node(-1,block_in) /* Excluding c_forbidden ~ content */
//[208]   l-explicit-document                ::= c-directives-end ( l-bare-document | ( e-node s-l-comments ) )
val l_explicit_document = c_directives_end ~ ( l_bare_document | ( e_node ~ s_l_comments ) )
//[209]   l-directive-document               ::= l-directive+ l-explicit-document
val l_directive_document = l_directive.rep(1) ~ l_explicit_document
//[210]   l-any-document                     ::= l-directive-document | l-explicit-document | l-bare-document
val l_any_document = l_directive_document | l_explicit_document | l_bare_document
//[211]   l-yaml-stream                      ::= l-document-prefix* l-any-document? ( l-document-suffix+ l-document-prefix* l-any-document? | l-document-prefix* l-explicit-document? )*
val l_yaml_stream = l_document_prefix.rep ~ l_any_document.? ~ ( l_document_suffix.rep(1) ~ l_document_prefix.rep ~ l_any_document.? | l_document_prefix.rep ~ l_explicit_document.? ).rep
}
