package fastparse.yaml

import fastparse.all._

/**
  * Context free Parsers.
  */
object YamlParsers {
  //def Y(indentation:Int, context:Context) = YamlParser(indentation,context)

  //TODO: BlockScalar instance caching
  //def BlockScalar(chomping:Chomping)(indentation:Int) = new BlockScalar(chomping,indentation)
  def BlockScalar(chomping:Chomping, indentation:Int) = new BlockScalar(chomping,indentation)
  def BlockScalar(t:(Chomping, Int)) = new BlockScalar(t._1,t._2)

  // for .flatten
  implicit def tuple2ToTraversible(t:(String,String)) = t.productIterator  
  implicit def mkString(t:(String,String)) = t._1 + t._2
  implicit def mkString(t:(String,String,String)) = t._1 + t._2 + t._3
  //private def flatten(t:(A,(B,C))):(A,B,C) = (t._1, t._2._1 + t._2._2)

  //[1]     c-printable                        ::= #x9 | #xA | #xD | [#x20-#x7E] /* 8 bit */ | #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD] /* 16 bit */ | [#x10000-#x10FFFF] /* 32 bit */
  val c_printable = CharIn(List('\u0009','\u000A','\u000D','\u0085'), ('\u0020' to '\u007E'), ('\u00A0' to '\uD7FF'), ('\uE000' to '\uFFFD'))
  //[2]     nb-json                            ::= #x9 | [#x20-#x10FFFF]
  private val nb_json = CharIn(List('\u0009'), '\u0020' to '\uFFFF').!.opaque("nb_json")
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
  lazy val indicator = CharIn("-?:,[]{}#&*!|>'\"%@`")
  //[23]    c-flow-indicator                   ::= "," | "[" | "]" | "{" | "}"
  val c_flow_indicator = ",[]{}"
  //[24]    b-line-feed                        ::= #xA /* LF */
  val b_line_feed = "\u000A"
  //[25]    b-carriage-return                  ::= #xD /* CR */
  val b_carriage_return = "\u000D"
  //[26]    b-char                             ::= b-line-feed | b-carriage-return
  val b_char = b_line_feed | b_carriage_return
  //[27]    nb-char                            ::= c-printable - b-char - c-byte-order-mark
  val nb_char = CharIn(List('\u0009', '\u0085'), '\u0020' to '\u007E', ('\u00A0' to '\uD7FF'), ('\uE000' to '\uFFFD').diff(c_byte_order_mark)).opaque("nb_char")
  //[28]    b-break                            ::= ( b-carriage-return b-line-feed ) /* DOS, Windows */ | b-carriage-return /* MacOS upto 9.x */ | b-line-feed /* UNIX, MacOS X */
  val b_break = ("\n"|"\r\n"|"\r").opaque("b_break") //((b_carriage_return ~ b_line_feed) | b_carriage_return | b_line_feed)
  //[29]    b-as-line-feed                     ::= b-break
  val b_as_line_feed = (b_break.map(Unit => "\u000A")).opaque("b_as_line_feed")
  //[30]    b-non-content                      ::= b-break
  val b_non_content = b_break
  //[31]    s-space                            ::= #x20 /* SP */
  val s_space = "\u0020"
  //[32]    s-tab                              ::= #x9 /* TAB */
  val s_tab = "\u0009"
  //[33]    s-white                            ::= s-space | s-tab
  val s_white = CharIn(" \t").opaque("s_white")
  val s_white_rep = s_white.rep.opaque("s_white_rep")
  val s_white_rep1 = s_white.rep(1).opaque("s_white_rep1")

  //[34]    ns-char                            ::= nb-char - s-white
  val ns_char = CharIn(('\u0021' to '\u007E'), ('\u00A0' to '\uD7FF'), ('\uE000' to '\uFFFD').diff(c_byte_order_mark)).opaque("ns_char")
  // ns-char - c-flow-indicator
  val ns_char_c_flow_indicator = CharIn(('\u0021' to '\u007E').diff(c_flow_indicator), ('\u00A0' to '\uD7FF'), ('\uE000' to '\uFFFD').diff(c_byte_order_mark)).opaque("ns_char - c_flow_indicator")
  //[35]    ns-dec-digit                       ::= [#x30-#x39] /* 0-9 */
  val ns_dec_digit = CharIn("0123456789")
  //[36]    ns-hex-digit                       ::= ns-dec-digit | [#x41-#x46] /* A-F */ | [#x61-#x66] /* a-f */
  val ns_hex_digit = CharIn("0123456789ABCDEFabcdef")
  //[37]    ns-ascii-letter                    ::= [#x41-#x5A] /* A-Z */ | [#x61-#x7A] /* a-z */
  val ns_ascii_letter = CharIn(('\u0041' to '\u005A'), ('\u0061' to '\u007A'))
  //[38]    ns-word-char                       ::= ns-dec-digit | ns-ascii-letter | "-"
  val ns_word_char = ns_dec_digit | ns_ascii_letter | "-"
  //[39]    ns-uri-char                        ::= "%" ns-hex-digit ns-hex-digit | ns-word-char | "#" | ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | "," | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")" | "[" | "]"
  val ns_uri_char = ("%" ~ ns_hex_digit ~ ns_hex_digit | ns_word_char | CharIn("#;/?:@&=+$,_.!~*'()[]"))
  //[40]    ns-tag-char                        ::= ns-uri-char - "!" - c-flow-indicator
  val ns_tag_char = (!(c_flow_indicator+"!") ~ ns_uri_char)
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
  //val ns_esc_8_bit = "x" ~ ( ns_hex_digit.rep(min=2,max=2) )
  //[60]    ns-esc-16-bit                      ::= "u" ( ns-hex-digit × 4 )
  //val ns_esc_16_bit = "u" ~ ( ns_hex_digit.rep(min=4,max=4) )
  //[61]    ns-esc-32-bit                      ::= "U" ( ns-hex-digit × 8 )
  //val ns_esc_32_bit = "U" ~ ( ns_hex_digit.rep(min=8,max=8) )
  //[62]    c-ns-esc-char                      ::= "\" ( ns-esc-null | ns-esc-bell | ns-esc-backspace | ns-esc-horizontal-tab | ns-esc-line-feed | ns-esc-vertical-tab | ns-esc-form-feed | ns-esc-carriage-return | ns-esc-escape | ns-esc-space | ns-esc-double-quote | ns-esc-slash | ns-esc-backslash | ns-esc-next-line | ns-esc-non-breaking-space | ns-esc-line-separator | ns-esc-paragraph-separator | ns-esc-8-bit | ns-esc-16-bit | ns-esc-32-bit )
  //val c_ns_esc_char = "\" ~ ( ns_esc_null | ns_esc_bell | ns_esc_backspace | ns_esc_horizontal_tab | ns_esc_line_feed | ns_esc_vertical_tab | ns_esc_form_feed | ns_esc_carriage_return | ns_esc_escape | ns_esc_space | ns_esc_double_quote | ns_esc_slash | ns_esc_backslash | ns_esc_next_line | ns_esc_non_breaking_space | ns_esc_line_separator | ns_esc_paragraph_separator | ns_esc_8_bit | ns_esc_16_bit | ns_esc_32_bit )
  private val ns_esc_map = Map(
    "0" -> "\u0000",
    "a" -> "\u0007",
    "b" -> "\u0008",
    "t" -> "\u0009",
    "\t" -> "\u0009",
    "n" -> "\n",
    //...
    "\"" -> "\"",
    "\\" -> "\\"
    //TODO: map all escaped chars to values
  )

  private def st = (s:String) => {println("****<"+s+">****"); s;}
  private def sq = (s:Seq[Any]) => {println("****<"+(s.mkString)+">****"); s;}

  private val c_ns_esc_char_escaped = P("\\" ~/ CharIn("0abt\tnvfre \"/\\N_LP").!.map(ns_esc_map))//.opaque("c_ns_esc_char_escaped")
  private val c_ns_esc_char_unicode = P(
    "\\x" ~/ ns_hex_digit.rep(min=2,max=2).!.map(Integer.parseInt(_, 4).toChar.toString) |
    "\\u" ~/ ns_hex_digit.rep(min=4,max=4).!.map(Integer.parseInt(_, 8).toChar.toString)
  /*| "\\U" ~ hexDigit(8)*/)
  val c_ns_esc_char:Parser[String] = P(c_ns_esc_char_escaped | c_ns_esc_char_unicode)//.opaque("c_ns_esc_char")
   


  //[63]    s-indent(n)                        ::= s-space × n
  def indent(indentation:Int) = s_space.rep(min=indentation,max=indentation)
  // like indent, but matches on any increase in indentation size and returns new indentation size
  def indent_more(indentation:Int):Parser[Int] = s_space.!.rep(indentation+1).map(_.length)
  // like indent, but matches on any positive indentation size and returns new indentation size
  val indent_any:Parser[Int] = s_space.!.rep(1).map(_.length)


  //[66]    s-separate-in-line                 ::= s-white+ | /* Start of line */
  val separate_in_line = s_white_rep1 //TODO: how to test for Start of line? identify cases where this matters

  //[72]    b-as-space                         ::= b-break
  val b_as_space = b_break.map(Unit=>" ")

  //[75]    c-nb-comment-text                  ::= "#" nb-char*
  val c_nb_comment_text = "#" ~ nb_char.rep
  //[76]    b-comment                          ::= b-non-content | /* End of file */
  val b_comment = b_non_content | End
  //[77]    s-b-comment                        ::= ( s-separate-in-line c-nb-comment-text? )? b-comment
  val s_b_comment = (( separate_in_line ~ c_nb_comment_text.? ).? ~ b_comment).opaque("s_b_comment")
  //[78]    l-comment                          ::= s-separate-in-line c-nb-comment-text? b-comment
  val l_comment = separate_in_line ~ c_nb_comment_text.? ~ b_comment
  //[79]    s-l-comments                       ::= ( s-b-comment | /* Start of line */ ) l-comment*
  val comments = ( s_b_comment /*|  Start of line */ ) ~ l_comment.rep //TODO: how to test for Start of line? identify cases where this matters


  //[89]    c-tag-handle                       ::= c-named-tag-handle | c-secondary-tag-handle | c-primary-tag-handle
  // [90]    c-primary-tag-handle               ::= "!"
  // [91]    c-secondary-tag-handle             ::= "!" "!"
  // [92]    c-named-tag-handle                 ::= "!" ns-word-char+ "!"
  val c_tag_handle = ("!" ~ ns_word_char.rep(1) ~ "!") | "!!" | "!"

  //[82]    l-directive                        ::= "%" ( ns-yaml-directive | ns-tag-directive | ns-reserved-directive ) s-l-comments
  val l_directive = {
    //[83]    ns-reserved-directive              ::= ns-directive-name ( s-separate-in-line ns-directive-parameter )*
    // [84]    ns-directive-name                  ::= ns-char+
    // [85]    ns-directive-parameter             ::= ns-char+
    val ns_reserved_directive = ns_char.rep(1) ~ ( separate_in_line ~ ns_char.rep(1) ).rep

    //[86]    ns-yaml-directive                  ::= "Y" "A" "M" "L" s-separate-in-line ns-yaml-version
    // [87]    ns-yaml-version                    ::= ns-dec-digit+ "." ns-dec-digit+
    val ns_yaml_version = ns_dec_digit.rep(1) ~ "." ~ ns_dec_digit.rep(1)
    val ns_yaml_directive = "YAML" ~ separate_in_line ~ ns_yaml_version

    //[88]    ns-tag-directive                   ::= "T" "A" "G" s-separate-in-line c-tag-handle s-separate-in-line ns-tag-prefix
    // [93]    ns-tag-prefix                      ::= c-ns-local-tag-prefix | ns-global-tag-prefix
    //  [94]    c-ns-local-tag-prefix              ::= "!" ns-uri-char*
    //  [95]    ns-global-tag-prefix               ::= ns-tag-char ns-uri-char*
    val ns_tag_prefix = ("!" ~ ns_uri_char.rep) | (ns_tag_char ~ ns_uri_char.rep)
    val ns_tag_directive = P("TAG" ~ separate_in_line ~ c_tag_handle ~ separate_in_line ~ ns_tag_prefix)

    "%" ~ ( ns_yaml_directive | ns_tag_directive | ns_reserved_directive ) ~ comments
  }

  //[97]    c-ns-tag-property                  ::= c-verbatim-tag | c-ns-shorthand-tag | c-non-specific-tag
  // [98]    c-verbatim-tag                     ::= "!" "<" ns-uri-char+ ">"
  // [99]    c-ns-shorthand-tag                 ::= c-tag-handle ns-tag-char+
  // [100]   c-non-specific-tag                 ::= "!"
  val c_ns_tag_property = ("!<" ~/ ns_uri_char.rep(1) ~ ">") | (c_tag_handle ~ ns_tag_char.rep(1)) | "!"

  //[102]   ns-anchor-char                     ::= ns-char - c-flow-indicator
  //[103]   ns-anchor-name                     ::= ns-anchor-char+
  lazy val ns_anchor_name = ns_char_c_flow_indicator.rep(1)
  //[101]   c-ns-anchor-property               ::= "&" ns-anchor-name
  val c_ns_anchor_property = P("&" ~ ns_anchor_name).map(_=>throw new UnsupportedOperationException("c_ns_anchor_property"))
  //[104]   c-ns-alias-node                    ::= "*" ns-anchor-name
  val c_ns_alias_node = P("*" ~ ns_anchor_name).map(_=>throw new UnsupportedOperationException("c_ns_anchor_property"))

  //[105]   e-scalar                           ::= /* Empty */
  val e_scalar:Parser[String] = Pass.map(Unit=>"")
  //[106]   e-node                             ::= e-scalar
  val e_node = e_scalar
  //[107]   nb-double-char                     ::= c-ns-esc-char | ( nb-json - "\" - """ )
  val nb_double_char:Parser[String] = c_ns_esc_char | ( !(CharIn("\\-\"")) ~ nb_json  )
  //[108]   ns-double-char                     ::= nb-double-char - s-white
  val ns_double_char = !(s_white) ~ nb_double_char

  //[111]   nb-double-one-line                 ::= nb-double-char*
  val nb_double_one_line:Parser[Seq[String]] = nb_double_char.rep
  //[114]   nb-ns-double-in-line               ::= ( s-white* ns-double-char )*
  val nb_ns_double_in_line:Parser[Seq[String]] = ( s_white_rep ~ ns_double_char ).rep

  //[117]   c-quoted-quote                     ::= "'" "'"
  //val c_quoted_quote = "''".map(Unit=>"'")
  //[118]   nb-single-char                     ::= c-quoted-quote | ( nb-json - "'" )
  val nb_single_char = (P("''").map(Unit=>"'") | ( !("'") ~ nb_json ))
  //[119]   ns-single-char                     ::= nb-single-char - s-white
  val ns_single_char:Parser[String] = !(s_white) ~ nb_single_char

  //[122]   nb-single-one-line                 ::= nb-single-char*
  val nb_single_one_line:Parser[Seq[String]] = nb_single_char.rep
  //[123]   nb-ns-single-in-line               ::= ( s-white* ns-single-char )*
  //val nb_ns_single_in_line:Parser[Seq[String]] = ( s_white_rep ~ ns_single_char ).rep
  // nb_json - s_white - "'" | "''"
  
  //[128]   ns-plain-safe-out                  ::= ns-char
  val ns_plain_safe_out = ns_char
  //[129]   ns-plain-safe-in                   ::= ns-char - c-flow-indicator
  val ns_plain_safe_in = ns_char_c_flow_indicator


  //[202]   l-document-prefix                  ::= c-byte-order-mark? l-comment*
  ///val l_document_prefix = c_byte_order_mark.? ~ l_comment.rep
  //[203]   c-directives-end                   ::= "-" "-" "-"
  ///val c_directives_end = "---"
  //[204]   c-document-end                     ::= "." "." "."
  ///val c_document_end = "..."
  //[205]   l-document-suffix                  ::= c-document-end s-l-comments
  ///val l_document_suffix = c_document_end ~ comments
  //[206]   c-forbidden                        ::= /* Start of line */ ( c-directives-end | c-document-end ) ( b-char | s-white | /* End of file */ )
  ///val c_forbidden = /* Start of line */ ( c_directives_end | c_document_end ) ~ ( b_char | s_white | End )

  //[207]   l-bare-document                    ::= s-l+block-node(-1,block-in) /* Excluding c-forbidden content */
  ///val l_bare_document = !(c_forbidden) ~ YamlParser(-1,BlockIn).s_l_block_node

  //[208]   l-explicit-document                ::= c-directives-end ( l-bare-document | ( e-node s-l-comments ) )
  ///val l_explicit_document = c_directives_end ~ ( l_bare_document | ( e_node ~ comments ) )
  //[209]   l-directive-document               ::= l-directive+ l-explicit-document
  ///val l_directive_document = l_directive.rep(1) ~ l_explicit_document
  //[210]   l-any-document                     ::= l-directive-document | l-explicit-document | l-bare-document
  ///val l_any_document = l_directive_document | l_explicit_document | l_bare_document
  //[211]   l-yaml-stream                      ::= l-document-prefix* l-any-document? ( l-document-suffix+ l-document-prefix* l-any-document? | l-document-prefix* l-explicit-document? )*
  ///val l_yaml_stream = l_document_prefix.rep ~ l_any_document.? ~ ( l_document_suffix.rep(1) ~ l_document_prefix.rep ~ l_any_document.? | l_document_prefix.rep ~ l_explicit_document.? ).rep
}

object YamlParser { 
  //TODO: YamlParser instance caching 
  def apply(indentation:Int, context:Context) = new YamlParser(indentation,context)
}

abstract class Context
case object BlockOut extends Context 
case object BlockIn extends Context
case object FlowOut extends Context
case object FlowIn extends Context
case object BlockKey extends Context
case object FlowKey extends Context

class Chomping
case object Strip extends Chomping
case object Clip extends Chomping
case object Keep extends Chomping

/** 
 *  Limitations:
 * - Representation dependent.
 * - No UTF-32 support.
 * 
 * @see http://www.yaml.org/spec/1.2/spec.html
 */
class YamlParser (val indentation:Int, val context:Context) {
  def apply(context:Context) = if(context==this.context) this else YamlParser(this.indentation,context)
  def apply(indentation:Int) = if(indentation==this.indentation) this else YamlParser(indentation,this.context)
  def +(indentation:Int) = YamlParser(this.indentation + indentation, this.context)

  import YamlParsers._


  //[63]    s-indent(n)                        ::= s-space × n
  val indent = YamlParsers.indent(indentation)
  // like s_indent, but matches on any increase in indentation size and returns new indentation size
  val indent_more:Parser[Int] = YamlParsers.indent_more(indentation)

  //[68]    s-block-line-prefix(n)             ::= s-indent(n)
  //val s_block_line_prefix = s_indent
  //[69]    s-flow-line-prefix(n)              ::= s-indent(n) s-separate-in-line?
  private val s_flow_line_prefix = s_space.rep(min=indentation)
/*
  //[67]    s-line-prefix(n,c)                 ::= c = block-out ⇒ s-block-line-prefix(n)
  //                                               c = block-in  ⇒ s-block-line-prefix(n)
  //                                               c = flow-out  ⇒ s-flow-line-prefix(n)
  //                                               c = flow-in   ⇒ s-flow-line-prefix(n)
  // val s_line_prefix = this.context match {
  //   case BlockOut => s_block_line_prefix
  //   case BlockIn  => s_block_line_prefix
  //   case FlowOut  => s_flow_line_prefix
  //   case FlowIn   => s_flow_line_prefix
  // }
 */
  //[70]    l-empty(n,c)                       ::= ( s-line-prefix(n,c) | s-indent(<n) ) b-as-line-feed
  // val l_empty = context match {
  //   case BlockOut => l_empty_Block
  //   case BlockIn  => l_empty_Block
  //   case FlowOut  => l_empty_Flow
  //   case FlowIn   => l_empty_Flow
  // }
  //[71]    b-l-trimmed(n,c)                   ::= b-non-content l-empty(n,c)+
  //val b_l_trimmed = (b_non_content ~ l_empty.rep(1))

  //[73]    b-l-folded(n,c)                    ::= b-l-trimmed(n,c) | b-as-space
  //val b_l_folded = b_l_trimmed | b_as_space
  //[74]    s-flow-folded(n)                   ::= s-separate-in-line? b-l-folded(n,flow-in) s-flow-line-prefix(n)

  //b_non_content=((b_carriage_return ~ b_line_feed) | b_carriage_return | b_line_feed)
   
  private lazy val s_flow_folded:Parser[String] = {
    val l_empty_Flow =  s_space.rep ~ b_non_content // " *\n"
    val b_l_folded_Flow = b_non_content ~ l_empty_Flow.!.rep(1).map("\\n" * _.length) | b_as_space
    separate_in_line.? ~ b_l_folded_Flow ~ s_flow_line_prefix
  }
  // separate_in_line ::= s-white+ | /* Start of line */

  //[80]    s-separate(n,c)                    ::= c = block-out ⇒ s-separate-lines(n)
  //                                               c = block-in  ⇒ s-separate-lines(n)
  //                                               c = flow-out  ⇒ s-separate-lines(n)
  //                                               c = flow-in   ⇒ s-separate-lines(n)
  //                                               c = block-key ⇒ s-separate-in-line
  //                                               c = flow-key  ⇒ s-separate-in-line
  //[81]    s-separate-lines(n)                ::= ( s-l-comments s-flow-line-prefix(n) ) | s-separate-in-line
  val separate = context match {
    case BlockOut | BlockIn | FlowOut | FlowIn => (( comments ~ s_flow_line_prefix ) | separate_in_line)
    case BlockKey | FlowKey => separate_in_line
  }
  //private lazy val separate_lines = ( comments ~ s_flow_line_prefix ) | separate_in_line

  //[96]    c-ns-properties(n,c)               ::= ( c-ns-tag-property ( s-separate(n,c) c-ns-anchor-property )? ) | ( c-ns-anchor-property ( s-separate(n,c) c-ns-tag-property )? )
  val properties:Parser[Unit] = ( c_ns_tag_property ~ ( separate ~ c_ns_anchor_property ).? ) | ( c_ns_anchor_property ~ ( separate ~ c_ns_tag_property ).? )

  lazy val quoted:Parser[String] = {
    val break = (s_white_rep ~ b_non_content ~ s_white_rep)
    val pbreak = break.!.rep(min=2).map(s=>"\n" * (s.length-1))
    val lbreak = break.map(n=> " ")
    val c_single_quoted = {
      //[121]   nb-single-text(n,c)                ::= c = flow-out  ⇒ nb-single-multi-line(n)
      //                                               c = flow-in   ⇒ nb-single-multi-line(n)
      //                                               c = block-key ⇒ nb-single-one-line
      //                                               c = flow-key  ⇒ nb-single-one-line
      //[124]   s-single-next-line(n)              ::= s-flow-folded(n) ( ns-single-char nb-ns-single-in-line ( s-single-next-line(n) | s-white* ) )?
      //[125]   nb-single-multi-line(n)            ::= nb-ns-single-in-line ( s-single-next-line(n) | s-white* )
      val nb_single_text:Parser[String] = context match {
        case FlowOut  | FlowIn  => {
          val nb_ns_single_in_line_char = CharIn('\u0021' to '\u0026','\u0028' to '\uFFFF')
          val nb_ns_single_in_line:Parser[String] = (nb_ns_single_in_line_char.rep(1).! | P("''").map(Unit=>"'") | pbreak | lbreak | s_white_rep1.!).rep.map(_.mkString);
          (( pbreak | lbreak | s_white_rep.! ) ~ nb_ns_single_in_line).map(mkString)
        }
        case BlockKey | FlowKey => 
          (CharIn('\u0021' to '\u0026','\u0028' to '\uFFFF',List('\t',' ')).rep(1).! | P("''").map(Unit=>"'")).rep.map(_.mkString)
      }
      //[120]   c-single-quoted(n,c)               ::= "'" nb-single-text(n,c) "'"
      "'" ~/ nb_single_text ~ "'"
    }

    //[109]   c-double-quoted(n,c)               ::= """ nb-double-text(n,c) """
    val c_double_quoted:Parser[String] = {
      //[110]   nb-double-text(n,c)                ::= c = flow-out  ⇒ nb-double-multi-line(n)
      //                                               c = flow-in   ⇒ nb-double-multi-line(n)
      //                                               c = block-key ⇒ nb-double-one-line
      //                                               c = flow-key  ⇒ nb-double-one-line
      val nb_double_text = context match {
        case FlowOut  | FlowIn  => {
          val double_char = CharIn(List('\u0021'),'\u0023' to '\u005B','\u005D' to '\uFFFF')
          val x:Parser[String] = (double_char.rep(1).! | c_ns_esc_char | pbreak | lbreak | s_white_rep1.!)
          val double_line:Parser[String] = x.rep.map(_.mkString);
          (( pbreak | lbreak | s_white_rep.! ) ~ double_line).map(mkString)
        }
        case BlockKey | FlowKey =>
          (CharIn(List('\t','\u0021','\u0020'),'\u0023' to '\u005B','\u005D' to '\uFFFF').rep(1).! | c_ns_esc_char).rep.map(_.mkString)
      }
      "\"" ~/ nb_double_text ~ "\""
    }

    c_single_quoted | c_double_quoted
  }

  //[127]   ns-plain-safe(c)                   ::= c = flow-out  ⇒ ns-plain-safe-out
  //                                               c = flow-in   ⇒ ns-plain-safe-in
  //                                               c = block-key ⇒ ns-plain-safe-out
  //                                               c = flow-key  ⇒ ns-plain-safe-in
  lazy val plain_safe = context match {
    case FlowOut | BlockKey => ns_plain_safe_out
    case FlowIn  | FlowKey  => ns_plain_safe_in
  }


  //[131]   ns-plain(n,c)                      ::= c = flow-out  ⇒ ns-plain-multi-line(n,c)
  //                                               c = flow-in   ⇒ ns-plain-multi-line(n,c)
  //                                               c = block-key ⇒ ns-plain-one-line(c)
  //                                               c = flow-key  ⇒ ns-plain-one-line(c)
  lazy val plain = {
    //[126]   ns-plain-first(c)                  ::= ( ns-char - c-indicator ) | ( ( "?" | ":" | "-" ) /* Followed by an ns-plain-safe(c)) */ )
    val amp_plain_safe = &(plain_safe)
    val ns_plain_first = ( !indicator ~ ns_char ) | ( CharIn("?:-") ~ amp_plain_safe)
    //[130]   ns-plain-char(c)                   ::= ( ns-plain-safe(c) - ":" - "#" ) | ( /* An ns-char preceding */ "#" ) | ( ":" /* Followed by an ns-plain-safe(c) */ )
    val ns_plain_char = (!CharIn(":#") ~ plain_safe) | /*TODO: An ns-char preceding “#” */ Fail | ":" ~ amp_plain_safe
    //[132]   nb-ns-plain-in-line(c)             ::= ( s-white* ns-plain-char(c) )*
    val nb_ns_plain_in_line = ( s_white_rep ~ ns_plain_char ).rep
    //[133]   ns-plain-one-line(c)               ::= ns-plain-first(c) nb-ns-plain-in-line(c)
    val ns_plain_one_line = (ns_plain_first ~ nb_ns_plain_in_line)
    //[134]   s-ns-plain-next-line(n,c)          ::= s-flow-folded(n) ns-plain-char(c) nb-ns-plain-in-line(c)
    //val s_ns_plain_next_line = s_flow_folded ~ ns_plain_char ~ nb_ns_plain_in_line
    //[135]   ns-plain-multi-line(n,c)           ::= ns-plain-one-line(c) s-ns-plain-next-line(n,c)*
    //val ns_plain_multi_line:Parser[String] =
    context match {
      case FlowOut  | FlowIn  => ns_plain_one_line ~ (s_flow_folded ~ ns_plain_char ~ nb_ns_plain_in_line)
      case BlockKey | FlowKey => ns_plain_one_line
    }
  }

  //[136]   in-flow(c)                         ::= c = flow-out  ⇒ flow-in
  //                                               c = flow-in   ⇒ flow-in
  //                                               c = block-key ⇒ flow-key
  //                                               c = flow-key  ⇒ flow-key
  lazy val in_flow = context match {
    case FlowOut | FlowIn => FlowIn
    case BlockKey | FlowKey => FlowKey
  }
/*
  //[137]   c-flow-sequence(n,c)               ::= "[" s-separate(n,c)? ns-s-flow-seq-entries(n,in-flow(c))? "]"
  val c_flow_sequence = "[" ~ separate.? ~ P(Y(in_flow).ns_s_flow_seq_entries).? ~ "]"
  //[138]   ns-s-flow-seq-entries(n,c)         ::= ns-flow-seq-entry(n,c) s-separate(n,c)? ( "," s-separate(n,c)? ns-s-flow-seq-entries(n,c)? )?
  val ns_s_flow_seq_entries = ns_flow_seq_entry.rep(sep=(separate.? ~ "," ~ separate.?)).! ~ separate.?
  //[139]   ns-flow-seq-entry(n,c)             ::= ns-flow-pair(n,c) | ns-flow-node(n,c)
  val ns_flow_seq_entry = ns_flow_pair | ns_flow_node
  //[140]   c-flow-mapping(n,c)                ::= "{" s-separate(n,c)? ns-s-flow-map-entries(n,in-flow(c))? "}"
  val c_flow_mapping = "{" ~ separate.? ~ P(Y(in_flow).ns_s_flow_map_entries).? ~ "}"
  //[141]   ns-s-flow-map-entries(n,c)         ::= ns-flow-map-entry(n,c) s-separate(n,c)? ( "," s-separate(n,c)? ns-s-flow-map-entries(n,c)? )?
  val ns_s_flow_map_entries = (ns_flow_map_entry ~ separate.? ~ ( "," ~ separate.? ~ ns_s_flow_map_entries.? ).?)
  //[142]   ns-flow-map-entry(n,c)             ::= ( "?" s-separate(n,c) ns-flow-map-explicit-entry(n,c) ) | ns-flow-map-implicit-entry(n,c)
  val ns_flow_map_entry = ( "?" ~ separate ~ ns_flow_map_explicit_entry ) | ns_flow_map_implicit_entry
  //[143]   ns-flow-map-explicit-entry(n,c)    ::= ns-flow-map-implicit-entry(n,c) | ( e-node /* Key */ e-node /* Value */ )
  val ns_flow_map_explicit_entry = ns_flow_map_implicit_entry | ( e_node.! ~ e_node.! )
  //[144]   ns-flow-map-implicit-entry(n,c)    ::= ns-flow-map-yaml-key-entry(n,c) | c-ns-flow-map-empty-key-entry(n,c) | c-ns-flow-map-json-key-entry(n,c)
  val ns_flow_map_implicit_entry = ns_flow_map_yaml_key_entry | c_ns_flow_map_empty_key_entry | c_ns_flow_map_json_key_entry
  //[145]   ns-flow-map-yaml-key-entry(n,c)    ::= ns-flow-yaml-node(n,c) ( ( s-separate(n,c)? c-ns-flow-map-separate-value(n,c) ) | e-node )
  val ns_flow_map_yaml_key_entry = ns_flow_yaml_node ~ ( ( separate.? ~ c_ns_flow_map_separate_value ) | e_node.! )
  //[146]   c-ns-flow-map-empty-key-entry(n,c) ::= e-node /* Key */ c-ns-flow-map-separate-value(n,c)
  val c_ns_flow_map_empty_key_entry = e_node ~ c_ns_flow_map_separate_value
  //[147]   c-ns-flow-map-separate-value(n,c)  ::= ":" /* Not followed by an ns-plain-safe(c) */ ( ( s-separate(n,c) ns-flow-node(n,c) ) | e-node /* Value */ )
  val c_ns_flow_map_separate_value = ":" ~ !ns_plain_safe ~ ( ( separate ~ ns_flow_node ) | e_node )
  //[148]   c-ns-flow-map-json-key-entry(n,c)  ::= c-flow-json-node(n,c) ( ( s-separate(n,c)? c-ns-flow-map-adjacent-value(n,c) ) | e-node )
  val c_ns_flow_map_json_key_entry = c_flow_json_node ~ ( ( separate.? ~ c_ns_flow_map_adjacent_value ) | e_node )
  //[149]   c-ns-flow-map-adjacent-value(n,c)  ::= ":" ( ( s-separate(n,c)? ns-flow-node(n,c) ) | e-node ) /* Value */
  val c_ns_flow_map_adjacent_value = ":" ~ ( ( separate.? ~ ns_flow_node ) | e_node )

  //[150]   ns-flow-pair(n,c)                  ::= ( "?" s-separate(n,c) ns-flow-map-explicit-entry(n,c) ) | ns-flow-pair-entry(n,c)
  val ns_flow_pair = ( "?" ~ separate ~ ns_flow_map_explicit_entry ) | ns_flow_pair_entry
  //[151]   ns-flow-pair-entry(n,c)            ::= ns-flow-pair-yaml-key-entry(n,c) | c-ns-flow-map-empty-key-entry(n,c) | c-ns-flow-pair-json-key-entry(n,c)
  val ns_flow_pair_entry = ns_flow_pair_yaml_key_entry | c_ns_flow_map_empty_key_entry | c_ns_flow_pair_json_key_entry
  //[152]   ns-flow-pair-yaml-key-entry(n,c)   ::= ns-s-implicit-yaml-key(flow-key) c-ns-flow-map-separate-value(n,c)
  val ns_flow_pair_yaml_key_entry = Y(0,FlowKey).ns_s_implicit_yaml_key ~ c_ns_flow_map_separate_value
  //[153]   c-ns-flow-pair-json-key-entry(n,c) ::= c-s-implicit-json-key(flow-key) c-ns-flow-map-adjacent-value(n,c)
  val c_ns_flow_pair_json_key_entry = Y(0,FlowKey).c_s_implicit_json_key ~ c_ns_flow_map_adjacent_value
  //[154]   ns-s-implicit-yaml-key(c)          ::= ns-flow-yaml-node(n/a,c) s-separate-in-line? /* At most 1024 characters altogether */
  val ns_s_implicit_yaml_key = (ns_flow_yaml_node ~ separate_in_line.?) //TODO: At most 1024 characters altogether
  //[155]   c-s-implicit-json-key(c)           ::= c-flow-json-node(n/a,c) s-separate-in-line? /* At most 1024 characters altogether */
  val c_s_implicit_json_key = (c_flow_json_node ~ separate_in_line.?)
  //[156]   ns-flow-yaml-content(n,c)          ::= ns-plain(n,c)
  val ns_flow_yaml_content:Parser[String] = ns_plain
  //[157]   c-flow-json-content(n,c)           ::= c-flow-sequence(n,c) | c-flow-mapping(n,c) | c-single-quoted(n,c) | c-double-quoted(n,c)
  val c_flow_json_content = c_flow_sequence | c_flow_mapping | c_single_quoted | c_double_quoted
  //[158]   ns-flow-content(n,c)               ::= ns-flow-yaml-content(n,c) | c-flow-json-content(n,c)
  val ns_flow_content = ns_flow_yaml_content | c_flow_json_content
  //[159]   ns-flow-yaml-node(n,c)             ::= c-ns-alias-node | ns-flow-yaml-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-yaml-content(n,c) ) | e-scalar ) )
  val ns_flow_yaml_node = c_ns_alias_node | ns_flow_yaml_content | ( c_ns_properties ~ ( ( separate ~ ns_flow_yaml_content ) | e_scalar ) )
  //[160]   c-flow-json-node(n,c)              ::= ( c-ns-properties(n,c) s-separate(n,c) )? c-flow-json-content(n,c)
  val c_flow_json_node = ( c_ns_properties ~ separate ).? ~ c_flow_json_content
  //[161]   ns-flow-node(n,c)                  ::= c-ns-alias-node | ns-flow-content(n,c) | ( c-ns-properties(n,c) ( ( s-separate(n,c) ns-flow-content(n,c) ) | e-scalar ) )
  val ns_flow_node = c_ns_alias_node | ns_flow_content | ( c_ns_properties ~ ( ( separate ~ ns_flow_content ) | e_scalar ) )

 */

  //[162]   c-b-block-header(m,t)              ::= ( ( c-indentation-indicator(m) c-chomping-indicator(t) ) | ( c-chomping-indicator(t) c-indentation-indicator(m) ) ) s-b-comment

  val c_b_block_header:Parser[BlockScalar] = {
    //[163]   c-indentation-indicator(m)         ::= ns-dec-digit ⇒ m = ns-dec-digit - #x30
    //                                               /* Empty */  ⇒ m = auto-detect()
    val c_indentation_indicator:Parser[Int] = CharIn("123456789").!.map(Integer.parseInt).opaque("c_indentation_indicator")
    //[164]   c-chomping-indicator(t)            ::= "-"         ⇒ t = strip
    //                                               "+"         ⇒ t = keep
    //                                               /* Empty */ ⇒ t = clip
    val c_chomping_indicator:Parser[Chomping] =
      (   wspStr("-").map(Unit=>Strip)//.log("Strip")
        | wspStr("+").map(Unit=>Keep)//.log("Keep")
        | Pass.map(Unit=>Clip)//.log("Clip")
      )//.log("c_chomping_indicator")

    val c_b_block_header_detect_indent:Parser[Int] =
        Peek(s_b_comment ~ (s_space.rep ~ b_non_content).rep ~ s_space.!.rep(indentation+1) ~ nb_char).map(_.length)

    val block_header:Parser[(Chomping, Int)] =
      ( ( (c_indentation_indicator ~ c_chomping_indicator).map(_.swap)
        | (c_chomping_indicator ~/ (c_indentation_indicator | c_b_block_header_detect_indent)))
        ~ s_b_comment
      )

    block_header.map(BlockScalar).~/ //.log("c_b_block_header")
  }

/*
  //[183]   l+block-sequence(n)                ::= ( s-indent(n+m) c-l-block-seq-entry(n+m) )+ /* For some fixed auto-detected m > 0 */
  val l_block_sequence = ( s_indent(n+m) ~ c_l_block_seq_entry(n+m) ).rep(1)

  //[184]   c-l-block-seq-entry(n)             ::= "-" /* Not followed by an ns-char */ s-l+block-indented(n,block-in)
  val c_l_block_seq_entry = "-" ~ !(ns_char) ~/ s_l_block_indented(BlockIn)
  //[185]   s-l+block-indented(n,c)            ::= ( s-indent(m) ( ns-l-compact-sequence(n+1+m) | ns-l-compact-mapping(n+1+m) ) ) | s-l+block-node(n,c) | ( e-node s-l-comments )
  val s_l_block_indented_compact = ns_l_compact_sequence | ns_l_compact_mapping
  private def s_l_block_indented(c:Context) = s_indent_any.flatMap((i:Int)=>Y(indentation+1+i,c).s_l_block_indented_compact) | s_l_block_node | ( e_node ~ comments )

  //[186]   ns-l-compact-sequence(n)           ::= c-l-block-seq-entry(n) ( s-indent(n) c-l-block-seq-entry(n) )*
  val ns_l_compact_sequence = c_l_block_seq_entry.rep(min=1,sep=s_indent)
  //[187]   l+block-mapping(n)                 ::= ( s-indent(n+m) ns-l-block-map-entry(n+m) )+ /* For some fixed auto-detected m > 0 */
  val l_block_mapping = s_indent_more.flatMap(Y(_,context).ns_l_block_map_entry.rep(1))
  //[188]   ns-l-block-map-entry(n)            ::= c-l-block-map-explicit-entry(n) | ns-l-block-map-implicit-entry(n)
  val ns_l_block_map_entry = c_l_block_map_explicit_entry | ns_l_block_map_implicit_entry
  //[189]   c-l-block-map-explicit-entry(n)    ::= c-l-block-map-explicit-key(n) ( l-block-map-explicit-value(n) | e-node )
  val c_l_block_map_explicit_entry = c_l_block_map_explicit_key ~ ( l_block_map_explicit_value | e_node )
  //[190]   c-l-block-map-explicit-key(n)      ::= "?" s-l+block-indented(n,block-out)
  val c_l_block_map_explicit_key = "?" ~ s_l_block_indented(BlockOut)
  //[191]   l-block-map-explicit-value(n)      ::= s-indent(n) ":" s-l+block-indented(n,block-out)
  val l_block_map_explicit_value = s_indent ~ ":" ~  s_l_block_indented(BlockOut)
  //[192]   ns-l-block-map-implicit-entry(n)   ::= ( ns-s-block-map-implicit-key | e-node ) c-l-block-map-implicit-value(n)
  val ns_l_block_map_implicit_entry = ( ns_s_block_map_implicit_key | e_node ) ~ c_l_block_map_implicit_value
  //[193]   ns-s-block-map-implicit-key        ::= c-s-implicit-json-key(block-key) | ns-s-implicit-yaml-key(block-key)
  val ns_s_block_map_implicit_key_impl = (c_s_implicit_json_key | ns_s_implicit_yaml_key)
  val ns_s_block_map_implicit_key = P(Y(0,BlockKey).ns_s_block_map_implicit_key_impl)

  //[194]   c-l-block-map-implicit-value(n)    ::= ":" ( s-l+block-node(n,block-out) | ( e-node s-l-comments ) )
  val c_l_block_map_implicit_value = ":" ~ ( P(Y(BlockOut).s_l_block_node) | ( e_node ~ comments ) )
  //[195]   ns-l-compact-mapping(n)            ::= ns-l-block-map-entry(n) ( s-indent(n) ns-l-block-map-entry(n) )*
  val ns_l_compact_mapping = ns_l_block_map_entry ~ ( s_indent ~ ns_l_block_map_entry ).rep
  //[196]   s-l+block-node(n,c)                ::= s-l+block-in-block(n,c) | s-l+flow-in-block(n)
  val s_l_block_node = s_l_block_in_block | s_l_flow_in_block
  //[197]   s-l+flow-in-block(n)               ::= s-separate(n+1,flow-out) ns-flow-node(n+1,flow-out) s-l-comments
  val s_l_flow_in_block_impl = separate ~ ns_flow_node ~ comments
  //val s_l_flow_in_block_Optomization = &(comments | separate_in_line).flatMap(Unit=>Y(indentation+1,FlowOut).s_l_flow_in_block_impl)
  val s_l_flow_in_block = Y(indentation+1,FlowOut).s_l_flow_in_block_impl

  //[198]   s-l+block-in-block(n,c)            ::= s-l+block-scalar(n,c) | s-l+block-collection(n,c)
  val s_l_block_in_block = s_l_block_scalar | s_l_block_collection
 */

  //[199]   s-l+block-scalar(n,c)              ::= s-separate(n+1,c) ( c-ns-properties(n+1,c) s-separate(n+1,c) )? ( c-l+literal(n) | c-l+folded(n) )
  private val s_l_block_scalar_pre:Parser[Unit] = (separate ~ ( properties ~ separate ).?) | Pass
  val block_scalar:Parser[String] = {
    //[170]   c-l+literal(n)                     ::= "|" c-b-block-header(m,t) l-literal-content(n+m,t)
    //val c_l_literal:Parser[String] = "|" ~ c_b_block_header.flatMap(_.l_literal_content)
    val c_l_literal:Parser[String] = ("|" ~/ c_b_block_header.flatMap(_.l_literal_content)).log("c_l_literal")
    //[174]   c-l+folded(n)                      ::= ">" c-b-block-header(m,t) l-folded-content(n+m,t)
    val c_l_folded:Parser[String] = (">" ~/ c_b_block_header.flatMap(_.l_folded_content)).log("c_l_folded")

    (P(this(indentation+1).s_l_block_scalar_pre) ~ ( c_l_literal | c_l_folded )).log("block_scalar")
  }
  
 /*
  //[200]   s-l+block-collection(n,c)          ::= ( s-separate(n+1,c) c-ns-properties(n+1,c) )? s-l-comments ( l+block-sequence(seq-spaces(n,c)) | l+block-mapping(n) )
  val s_l_block_collection_properties_impl = (( separate ~ c_ns_properties ).?)
  val s_l_block_collection_properties = P(Y(indentation+1).s_l_block_collection_properties_impl)
  //val s_l_block_collection_properties_Optomization = &(comments | separate_in_line).flatMap(Unit=>Y(indentation+1).s_l_block_collection_properties_impl)
  //[201]   seq-spaces(n,c)                    ::= c = block-out ⇒ n-1
  //                                               c = block-in  ⇒ n
  val l_block_sequence_seq_spaces = context match {
      case BlockOut => Y(indentation-1,BlockOut).l_block_sequence
      case BlockIn => l_block_sequence
  }
  val s_l_block_collection = s_l_block_collection_properties ~ comments ~ ( l_block_sequence_seq_spaces | l_block_mapping )
 */
}


//object BlockScalar {
//}

class BlockScalar(val chomping:Chomping, val indentation:Int) {
  import YamlParsers._
  //val Y = YamlParser(indentation,BlockIn)
  private val s_indent = s_space.rep(min=indentation,max=indentation).opaque("s_indent("+indentation+")")
  private val l_empty_Block = s_space.rep(max=indentation) ~ b_non_content
  private val l_empty_rep = l_empty_Block.rep

  //private def blank(p:Parser[Unit]):Parser[String] = p.map(Unit=>"")

  //[64]    s-indent(<n)                       ::= s-space × m /* Where m < n */
  private val s_indent_lt = s_space.rep(min=0,max=indentation-1).opaque("s_indent(<"+indentation+")")
  //[65]    s-indent(≤n)                       ::= s-space × m /* Where m ≤ n */
  private val s_indent_le = s_space.rep(min=0,max=indentation).opaque("s_indent(≤"+indentation+")")

  //[165]   b-chomped-last(t)                  ::= t = strip ⇒ b-non-content | /* End of file */
  //                                               t = clip  ⇒ b-as-line-feed | /* End of file */
  //                                               t = keep  ⇒ b-as-line-feed | /* End of file */
  private val b_chomped_last:Parser[String] = End.map(Unit=>"") | (chomping match {
    case Strip => b_non_content.map(Unit=>"")
    case Clip  => b_as_line_feed
    case Keep  => b_as_line_feed
  })

  //[169]   l-trail-comments(n)                ::= s-indent(<n) c-nb-comment-text b-comment l-comment*
  private val l_trail_comments = s_indent_lt ~ c_nb_comment_text ~ b_comment ~ l_comment.rep
  //[167]   l-strip-empty(n)                   ::= ( s-indent(≤n) b-non-content )* l-trail-comments(n)?
  private val l_strip_empty = (( s_indent_le ~ b_non_content ).rep ~ l_trail_comments.?).map(Unit=>"")
  //[168]   l-keep-empty(n)                    ::= l-empty(n,block-in)* l-trail-comments(n)?
  private val l_keep_empty = (l_empty_rep.! ~ l_trail_comments.?)
  //[166]   l-chomped-empty(n,t)               ::= t = strip ⇒ l-strip-empty(n)
  //                                               t = clip  ⇒ l-strip-empty(n)
  //                                               t = keep  ⇒ l-keep-empty(n)
  private val l_chomped_empty:Parser[String] = chomping match {
    case Strip => l_strip_empty
    case Clip  => l_strip_empty
    case Keep  => l_keep_empty
  }

  // (space{0,n}\n)* space{n}[^\n\r]+
  // 
  val l_literal_content:Parser[String] = {
    //[171]   l-nb-literal-text(n)               ::= l-empty(n,block-in)* s-indent(n) nb-char+
    val l_nb_literal_text:Parser[String] = l_empty_rep ~ s_indent ~ nb_char.rep(1).!
    //[172]   b-nb-literal-next(n)               ::= b-as-line-feed l-nb-literal-text(n)
    val b_nb_literal_next:Parser[(String,String)] = b_as_line_feed ~ l_nb_literal_text;
    //[173]   l-literal-content(n,t)             ::= ( l-nb-literal-text(n) b-nb-literal-next(n)* b-chomped-last(t) )? l-chomped-empty(n,t)
    (( l_nb_literal_text ~ b_nb_literal_next.rep ~ b_chomped_last ).? ~ l_chomped_empty)//.log("l_literal_content")
      .map(t=>t._1.map(u=>u._1 + u._2.flatten.mkString + u._3).getOrElse("") + t._2);
  }

  lazy val l_folded_content:Parser[String] = {
    val b_l_folded_Block = ((b_non_content ~ l_empty_Block.!.rep(1).map("\n" * _.length)).opaque("b_l_trimmed") | b_as_space.opaque("b_as_space"))//.log("b_l_folded("+indentation+",Block)")
    //[175]   s-nb-folded-text(n)                ::= s-indent(n) ns-char nb-char*
    val s_nb_folded_text:Parser[String] = (s_indent ~ (ns_char ~ nb_char.rep).!)//.log("s_nb_folded_text")
    //[176]   l-nb-folded-lines(n)               ::= s-nb-folded-text(n) ( b-l-folded(n,block-in) s-nb-folded-text(n) )*
    val l_nb_folded_lines:Parser[(String,Seq[(String,String)])] = (s_nb_folded_text ~ ( b_l_folded_Block ~ s_nb_folded_text ).rep)//.log("folded_lines")
    //[177]   s-nb-spaced-text(n)                ::= s-indent(n) s-white nb-char*
    val s_nb_spaced_text = (s_indent ~ (s_white ~ nb_char.rep).!)//.log("s_nb_spaced_text")
    //[178]   b-l-spaced(n)                      ::= b-as-line-feed l-empty(n,block-in)*
    val b_l_spaced = b_as_line_feed ~ l_empty_rep
    //[179]   l-nb-spaced-lines(n)               ::= s-nb-spaced-text(n) ( b-l-spaced(n) s-nb-spaced-text(n) )*
    val l_nb_spaced_lines:Parser[(String,Seq[(String,String)])] = (s_nb_spaced_text ~ ( b_l_spaced ~ s_nb_spaced_text ).rep)//.log("l_nb_spaced_lines")
    //[180]   l-nb-same-lines(n)                 ::= l-empty(n,block-in)* ( l-nb-folded-lines(n) | l-nb-spaced-lines(n) )
    val l_nb_same_lines:Parser[String] = l_empty_rep ~ ( l_nb_folded_lines | l_nb_spaced_lines ).map(t=> t._1 + t._2.flatten.mkString)
    //[181]   l-nb-diff-lines(n)                 ::= l-nb-same-lines(n) ( b-as-line-feed l-nb-same-lines(n) )*
    val l_nb_diff_lines = l_nb_same_lines.rep(min=1,sep=b_as_line_feed).map(s=>s.mkString("\u000A"))
     //l_nb_same_lines ~ ( b_as_line_feed ~ l_nb_same_lines ).rep).map(t=>t._1 + t._2.flatten.mkString)
    //[182]   l-folded-content(n,t)              ::= ( l-nb-diff-lines(n) b-chomped-last(t) )? l-chomped-empty(n,t)
    (( l_nb_diff_lines ~ b_chomped_last ).? ~ l_chomped_empty).map(t=>t._1.map(mkString).getOrElse("") + t._2)
  }
}


