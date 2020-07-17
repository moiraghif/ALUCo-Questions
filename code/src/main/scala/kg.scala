package semantics


import scala.util.matching.Regex
import scala.collection.JavaConverters._

import org.apache.jena.rdf.model.RDFNode
import org.apache.jena.rdfconnection.{
  RDFConnection,
  RDFConnectionRemote }
import org.apache.jena.query.{
  QueryExecution,
  QueryExecutionFactory,
  QuerySolution }


import main.constants._
import main.utils._
import nlp.{
  Parser,
  Sentence }


object KGfunctions {

  // a general prefix for all queries
  val queryPrefix = """PREFIX owl:  <http://www.w3.org/2002/07/owl#>
                      |PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                      |PREFIX xml:  <http://www.w3.org/XML/1998/namespace#>
                      |PREFIX xsd:  <http://www.w3.org/2001/XMLSchema#>
                      |PREFIX text: <http://jena.apache.org/text#>
                      |""".stripMargin

  def connectToKg(url: String)(query: String): QueryExecution = {
    val queryFull = queryPrefix + query
    // if (printLog) println(queryFull)
    return QueryExecutionFactory.sparqlService(url, queryFull)
  }

  val url = getServerURL
  val queryKG = connectToKg(url) _


  def cleanUri(uri: Any): String = {
    // clean the URI from any prefix
    // (rdfs:isA) -> isA
    val pattern = "^.+#".r
    return pattern replaceFirstIn(uri.toString(), "")
  }


  def querySelect(query: String): Iterator[QuerySolution] = {
    // execute a QUERY with a SELECT statement
    // return an iterator of possible solutions
    val solutions = queryKG(query).execSelect().asScala
    Thread.sleep(getLatency.toLong)
    for {solution <- solutions} yield solution
  }

}



object NEE {

  def getWithLanguage(variable: String, label: String, lang: String): String =
    s"""  {
       |    $variable ?r "$label"@$lang.
       |  } UNION {
       |    $variable ?r "$label".
       |  }
       |""".stripMargin


  def getCandidates(candidate: String): Iterator[QuerySolution] =
    semantics.KGfunctions.querySelect(
      s"""SELECT DISTINCT ?candidate WHERE {
         |  ?candidate rdfs:label ?label.
         |  FILTER(STRSTARTS(LCASE(?label), \"${candidate.toLowerCase}\"))
         |}""".stripMargin)


  def getEntities(subTree: Sentence): Array[QuerySolution] = {
    // get the array of entities for a string
    
    // exclude the string if has not a valid syntax construction
    if (! subTree.isValidTree)
      return Array[QuerySolution]()

    // other strings are analyzed in order to search for combinations of upper/lower cases
    if (printLog) println("checking: \"" + subTree.text.mkString(" ") + "\"")
    val query = "SELECT DISTINCT ?candidate WHERE {\n" + 
      binaryPermutations(subTree.length).map(
        candidatePerm => {
          val forma: String = subTree.text.zipWithIndex.map(
            i => {
              if (candidatePerm(i._2)) i._1.capitalize
              else i._1
            }).mkString(" ")
          getWithLanguage("?candidate", forma, subTree.lang)
        }).mkString(" UNION ") + "\n}"
    return semantics.KGfunctions.querySelect(query).toArray
  }


  def apply(tree: Sentence): Array[QuerySolution] = {
    // use a sliding window to map the TREE to find a topic entity
    var checked = List[Sentence]()
    var out = Array[QuerySolution]()
    var outScore = 0
    // from: http://universaldependencies.org/docs/u/pos/index.html
    val validPos: Array[String]  = Array("ADJ", "ADV", "INTJ", "NOUN", "PROPN", "VERB")
    for (windowSize <- (1 to Array[Int](tree.length - 1,
                                    getConfig("maximumWindow").toInt).min).reverse;
       i <- 0 to (tree.length - windowSize)) {
      // for each possible substring
      val candidate: Sentence = tree.getPortion((i, i + windowSize))
      if (checked.filter(isSubStringOf(candidate, _)).isEmpty &&
            (windowSize > 1 || (windowSize == 1 && validPos.contains(tree.pos(i))))) {
        val entities = getEntities(candidate).toArray
        if (! entities.isEmpty) {
          checked = checked :+ candidate
          val candidateScore = getTreeMaxDepth(candidate, tree)
          if (outScore <= candidateScore) {
            out = entities
            outScore = candidateScore
            if (printLog) println(s" - candidates: ${out.length}")
          }
        }
      }
    }
    // if desperated, check for single lemmas
    if (out.isEmpty)
      for (i <- 0 until tree.length)
        if (validPos.contains(tree.pos(i))) {
          val candidate = tree.getPortion(i, i + 1)
          val entities = getEntities(candidate)
          if (! entities.isEmpty) {
            if (printLog) println(s" - candidates: ${entities.length}")
            return entities
          }
        }
    return out
  }

}
