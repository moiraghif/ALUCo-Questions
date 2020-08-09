name := "SemanticQA";
version := "0.1";

scalacOptions := Seq("-unchecked", "-deprecation")


scalaVersion := "2.13.3";

val scalajVersion = "2.4.2";
val jsonVersion = "1.3.5";
val jenaVersion = "3.13.0";
val tikaVersion = "1.24.1";
val graphVersion = "1.13.2"


// JSON parser
libraryDependencies += "io.spray" %%  "spray-json" % jsonVersion;
libraryDependencies += "org.scalaj" % "scalaj-http_2.12" % scalajVersion;


// Apache Jena
libraryDependencies += "org.apache.jena" % "jena-core"  % jenaVersion;
libraryDependencies += "org.apache.jena" % "jena-arq"   % jenaVersion;
libraryDependencies += "org.apache.jena" % "jena-iri"   % jenaVersion;
libraryDependencies += "org.apache.jena" % "jena-fuseki-main" % jenaVersion;

// Apache Tika
libraryDependencies += "org.apache.tika" % "tika-core"        % tikaVersion;
libraryDependencies += "org.apache.tika" % "tika-langdetect"  % tikaVersion;


// Graphs
libraryDependencies += "org.scala-graph" % "graph-core_2.13" % graphVersion;
// resolvers += "twitter" at "http://maven.twttr.com"
// libraryDependencies += "com.twitter" %% "cassovary-core" % "7.1.0"
// libraryDependencies += "it.unimi.dsi" % "fastutil" % "7.0.7"
