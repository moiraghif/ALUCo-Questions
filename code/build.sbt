name := "Main";
version := "alphissima";

scalaVersion := "2.12.0";


val jsonVersion = "1.3.5";
val jenaVersion = "3.13.0";
val tikaVersion = "1.24.1";


libraryDependencies += "io.spray" %%  "spray-json" % jsonVersion;

libraryDependencies += "org.apache.jena" % "jena-core"  % jenaVersion;
libraryDependencies += "org.apache.jena" % "jena-arq"   % jenaVersion;
libraryDependencies += "org.apache.jena" % "jena-iri"   % jenaVersion;
libraryDependencies += "org.apache.jena" % "jena-fuseki-main" % jenaVersion;

libraryDependencies += "org.apache.tika" % "tika-core"        % tikaVersion;
libraryDependencies += "org.apache.tika" % "tika-langdetect"  % tikaVersion;
