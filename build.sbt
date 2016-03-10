name := "YAML fastparse"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "0.3.4",
  "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test"
)
excludeFilter in unmanagedSources := ".#*" //|| "RamlParsers.scala"
//excludeFilter in unmanagedSources := "RamlParsers.scala" || "YamlParsers.scala"
