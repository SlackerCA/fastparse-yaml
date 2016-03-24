/*
=Use Cases=
The YAML "schema" is already defined, and I need to read a conforming file.

By defining what is expected, we generate better error messages.
 - Missing required value.
 - Value is not expected type (map, sequence, or scalar).
 - Duplicate key found.
 - 


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
Maps are the backbone structure.

map(
"foo" -> string, // simple key-value pair. error if value is not a string.
"foo" -> sequence, // 
"bar" -> map(string->string).map(m:Map[String,String]=>new Foo(m)),
"annotations" -> collect("(.*)".r -> annotation) //Map[String,Annotation]
collect("\\?.*)".r -> optional)
).map(m:Map[AnyRef,AnyRef]=>


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


=======
Use a Parser[AnyRef], and keep the Success object so that we can still reference where in the source text the problem seems to be.
Exposes 

=Example=

document(
"title"->string,
"documentation" -> seq(map...)
"annotations"-> map(
(string.filter(_.startsWith("/")) -> map) match { case (k:String,v:Map[AnyRef,AnyRef]) if k.startsWith("/") => (k,v)}


) match {
case m:Map => // val Raml(m.get("title"),m.get("descirption"),m.get("annotations").map(
}


doc[A,B](Parser[A],Parser[B]):(A,B)
"title"->string => Parser[String]
"documentation"->map => Parser[Map]
"annotations"->map() =>

*/

package fastparse.yaml

import fastparse.all._

object Yaml {
}

