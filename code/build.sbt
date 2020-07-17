name := "SemanticQA";
version := "0.1";


scalaVersion := "2.12.0";

val jsonVersion = "1.3.5";
val jenaVersion = "3.13.0";
val tikaVersion = "1.24.1";


// JSON parser
libraryDependencies += "io.spray" %%  "spray-json" % jsonVersion;

// Apache Jena
libraryDependencies += "org.apache.jena" % "jena-core"  % jenaVersion;
libraryDependencies += "org.apache.jena" % "jena-arq"   % jenaVersion;
libraryDependencies += "org.apache.jena" % "jena-iri"   % jenaVersion;
libraryDependencies += "org.apache.jena" % "jena-fuseki-main" % jenaVersion;

// Apache Tika
libraryDependencies += "org.apache.tika" % "tika-core"        % tikaVersion;
libraryDependencies += "org.apache.tika" % "tika-langdetect"  % tikaVersion;
