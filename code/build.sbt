name := "SemanticQA";
version := "0.1";

scalacOptions := Seq("-unchecked", "-deprecation")


scalaVersion := "2.13.3";


val akkaVersion = "2.6.8"
val akkaHttpVersion = "10.2.0"
val scalajVersion = "2.4.2";
val jsonVersion = "1.3.5";
val jenaVersion = "3.16.0";
val tikaVersion = "1.24.1";
val graphVersion = "1.13.2"


// JSON parser
libraryDependencies += "io.spray" %%  "spray-json" % jsonVersion;
libraryDependencies += "org.scalaj" % "scalaj-http_2.13" % scalajVersion;

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % akkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % akkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-http" % akkaHttpVersion
libraryDependencies += "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion


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
