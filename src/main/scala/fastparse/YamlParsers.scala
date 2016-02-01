package fastparse.yaml

import fastparse.all._
import scala.util.matching.Regex
import scala.collection.immutable.NumericRange

// class Chars(val ranges:List[NumericRange[Char]], val neg:List[Char]) {
  
//   def -(neg:List[Char]):Chars = {
//     new Chars(ranges, neg ::: this.neg)
//   }
//   def parser = 
//     CharPred(c => !neg.contains(c) && ranges.exists(_.contains(c)))
// }

/** 
 * No UTF-32 encodings support.
 * @see http://www.yaml.org/spec/1.2/spec.html
 */
trait YamlParsers {
  //def between(from:Char,to:Char)(c:Char) = from <= c && c <= to

  //('\u00A0' <= c && c <= '\uD7FF') || ('\uE000' <= c && c <= '\uFFFD')

  // [1]	c-printable	::=	  #x9 | #xA | #xD | [#x20-#x7E]          /* 8 bit */
  //                                    | #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD] /* 16 bit */
  //                                    | [#x10000-#x10FFFF]                     /* 32 bit */
  /*val c_printable = List(List('\u0009','\u000A','\u000D','\u0085'), ('\u0020' to '\u007E'), ('\u00A0' to '\uD7FF'), ('\uE000' to '\uFFFD'))*/
  // the high end of c-printable which is usually included
  val c_printable_high = CharPred(c => ('\u00A0' <= c && c <= '\uD7FF') || ('\uE000' <= c && c <= '\uFFFD') || c == '\u0085')

  // [3]	c-byte-order-mark	::=	#xFEFF
  // [23]	c-flow-indicator	::=	“,” | “[” | “]” | “{” | “}”
  // [26]	b-char	::=	b-line-feed | b-carriage-return
  // [27]	nb-char	::=	c-printable - b-char - c-byte-order-mark
  // [28]	b-break	::=	  ( b-carriage-return b-line-feed ) | b-carriage-return | b-line-feed
  val b_break = "\\n\\r" | CharIn("\\n\\r")

  // c-printable - b-line-feed - b-carriage-return - c-byte-order-mark
  val nb_char = (CharIn(('\u0020' to '\u007E') :+ '\u0009') | c_printable_high).opaque("nb_char").log()

  // [33]	s-white	::=	s-space | s-tab
  val s_white = CharIn(" \\t").opaque("s_white")

  // [34]	ns-char	::=	nb-char - s-white
  // c-printable - b-line-feed - b-carriage-return - c-byte-order-mark - s-space - s-tab
  val ns_char = CharIn(('\u0021' to '\u007E')) | c_printable_high

  // [66]       s-separate-in-line      ::=     s-white+ | /* Start of line */
  val s_separate_in_line = s_white.rep(1) | Start
  // [75]	c-nb-comment-text	::=	“#” nb-char*
  // [76]	        b-comment	::=	b-non-content | /* End of file */
  // [77]	      s-b-comment       ::=     ( s-separate-in-line c-nb-comment-text? )? b-comment
  // ends a line in non scalar context 
  val s_b_comment = ( ((Start|s_white.rep(1)) ~ "#" ~ nb_char.rep).? ~ (b_break|End) ).opaque("s_b_comment").log()
  // [78]               l-comment       ::=     s-separate-in-line c-nb-comment-text? b-comment
  val l_comment = (Start|s_white.rep(1)) ~ ("#" ~ nb_char.rep).? ~ (b_break|End)
  // [79]            s-l-comments       ::=     ( s-b-comment | /* Start of line */ ) l-comment*
  val s_l_comments = (s_b_comment|Start) ~ l_comment.rep

  // [81]     s-separate-lines(n)       ::=     ( s-l-comments s-flow-line-prefix(n) ) | s-separate-in-line
  val s_separate_lines = ((s_l_comments ~ s_white.rep) | s_separate_in_line).opaque("s_separate_lines").log()

  // [126]      ns-plain-first(c)       ::=     ( ns-char - c-indicator )
  //                                            | ( ( “?” | “:” | “-” )
  //                                              /* Followed by an ns-plain-safe(c)) */ )

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
  val ns_plain_safe_in = CharIn(('\u0021' to '\u007E').diff(",[]{}")) | c_printable_high
  val ns_plain_char_flow = ( CharIn(('\u0021' to '\u007E').diff(",[]{}:#")) | c_printable_high
                           | (CharIn(",[]{}:#") ~ &("#"))
                           | P(":" ~ &(ns_plain_safe_in)) )

  val ns_plain_char_block = ( CharIn(('\u0021' to '\u007E').diff(":#"))
                            | CharIn(":#") ~ &("#")
                            | ":" ~ &(ns_char) )

  // [129]      ns-plain-safe-in        ::=     ns-char - c-flow-indicator

  //val ns_plain_safe_in = new Chars
  //val eol = CharIn("\\n\\r").rep.opaque("<line-break>")

  //val plain_first = CharIn("-?:,[]{}#&*!|>'\"%@`")

  // [132]      nb-ns-plain-in-line(c)  ::=     ( s-white* ns-plain-char(c) )*
  // [133]      ns-plain-one-line(c)    ::=     ns-plain-first(c) nb-ns-plain-in-line(c)
  // ns_plain_first ( s-white* ns-plain-char(c) )*
  // ( ns-char - c-indicator ) ( s-white* ns-plain-char(c) )* |
  // ( “?” | “:” | “-” ) ( s-white* ns-plain-char(c) )+
  // val ns_plain_one_line_block =
  val ns_plain_one_line_flow =
    ( (CharIn(('\u0021' to '\u007E').diff("-?:,[]{}#&*!|>'\"%@`")) | c_printable_high) ~ (s_white.rep ~ ns_plain_char).rep
    | (CharIn("?:-") ~ (s_white.rep ~ ns_plain_char).rep(1)) )


  // [137]       c-flow-sequence(n,c)   ::=     “[” s-separate(n,c)? ns-s-flow-seq-entries(n,in-flow(c))? “]”

  // [140]        c-flow-mapping(n,c)   ::=     “{” s-separate(n,c)? ns-s-flow-map-entries(n,in-flow(c))? “}”
  // [141] ns-s-flow-map-entries(n,c)   ::=     ns-flow-map-entry(n,c) s-separate(n,c)?
  //                                            ( “,” s-separate(n,c)? ns-s-flow-map-entries(n,c)? )?
  // [142]     ns-flow-map-entry(n,c)   ::=     ( “?” s-separate(n,c) ns-flow-map-explicit-entry(n,c) )
  //                                            | ns-flow-map-implicit-entry(n,c)

  val c_flow_mapping = P("{" ~ s_separate_lines.? ~ ns_s_flow_map_entries.? ~ "}").log()
  // ??? support non-scalar keys ???
  // FIXME
  val ns_flow_map_implicit_entry =  ns_plain_one_line_flow ~ ":" ~ !(ns_plain_safe_in) ~ ns_plain_one_line_flow
  val ns_flow_map_explicit_entry = ns_flow_map_implicit_entry //TODO: | map to null 

  val ns_flow_map_entry = P( "?" ~ s_separate_in_line ~ ns_flow_map_explicit_entry ) | ns_flow_map_implicit_entry

  val ns_s_flow_map_entries = (s_separate_in_line.? ~ ns_flow_map_entry.! ~ s_separate_in_line.?).rep(sep=",")

  //def escaped = P( "\\" ~ CharIn("0abt\tnvfre \"/\\N_LP") )  //.map()
  //def hexDigit(n:Int) = P(CharIn("abcdefABCDEF0123456789")).rep(min=n,max=n)
  //def unicode = P( "\\x" ~ hexDigit(2) | "\\u" ~ hexDigit(4) | "\\U" ~ hexDigit(8) )  //.map()
  //def doubleQuotedChars = P(CharsWhile(!"\"\\".contains(_:Char)))
  //def doubleQuoted = P("\"" ~/ (doubleQuotedChars | escaped | unicode).rep.! ~ "\"").log()
  //def singleQuoted = P("'" ~/ (!"'" | AnyChar).rep  ~ "'")
  //def plain = P(CharsWhile(!":\\n\\r".contains(_:Char)))
  //def string = P( doubleQuoted | singleQuoted | liternal )


  val ns_plain_char = P(CharsWhile(!":#".contains(_:Char)))
//| ( /* An ns-char preceding */ “#” )
//| ( “:” /* Followed by an ns-plain-safe(c) */ )
  //def scalar(key:String):Parser[(String,String)] = P((doubleQuoted|plain).! ~ ws ~ ":" ~/ ws ~ (doubleQuoted|plain).!).opaque(key).log()
  
  //def doubleQuoted: Parser[String] =
  //  P("\"" ~ ("""([^"\p{Cntrl}\\]|\\[0abt\tnvfre "/\\N_LP \\'"bfnrt]|\\x[a-fA-F0-9]{2}|\\u[a-fA-F0-9]{4}|\\U[a-fA-F0-9]{8})*+""".r).! ~ "\"")
  /*
  def comment: Parser[String] = "#.*".r
  def directive: Parser[String] = "![^!].*".r
  def tag: Parser[String] = "!!.*".r

  def singleQuoted: Parser[String] = ("'([^']|'')*'").r
    

  def key: Parser[String] = "\"" ~  ~ "\"" | 
  def scalar: Parser[Node] = str "':" str
  def seq: Parser[List[Node]]
  def map: Parser[Map[]]
 */
}

/*
sealed class Node(key:String)
case class ScalarNode(key:String, value:String) extends Node
case class SequenceNode(key:String, value:List[Node]) extends Node
case class MappingNode(key:String, value:Node) extends Node

*/
