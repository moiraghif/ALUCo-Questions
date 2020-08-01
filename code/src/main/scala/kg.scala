package semantics


import scala.util.matching.Regex
import scala.jdk.CollectionConverters._

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
  POS,
  Sentence }
// import semantics.RDFtranslation


object KG {

  /**
   * A general prefix for all queries
   * (the resolution process does not need it, but it is easier to read and
   * understand the rest of the source code) 
   */
  val queryPrefix = """PREFIX owl:  <http://www.w3.org/2002/07/owl#>
                      |PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                      |PREFIX xml:  <http://www.w3.org/XML/1998/namespace#>
                      |PREFIX xsd:  <http://www.w3.org/2001/XMLSchema#>
                      |""".stripMargin

  def connectToKg(url: String)(query: String): QueryExecution = {
    /**
     * Connect to a KG using via internet
     */
    val queryFull = queryPrefix + query
    return QueryExecutionFactory.sparqlService(url, queryFull)
  }

  val url = getServerURL
  val queryKG = connectToKg(url) _


  def apply(query: String): Iterator[QuerySolution] = {
    /**
     * execute a select QUERY from the standard database; it returns an iterator
     * of possible solutions
     * this function is polite: it respects the latency setted in config.json
     */
    // TODO: add try-catch in case the query does not succeed
    val solutions = queryKG(query).execSelect().asScala
    Thread.sleep(getLatency().toLong)
    for {solution <- solutions} yield solution
  }

}


object Lexicalization {
  def cleanUri(uri: RDFNode): String = {
    /**
     * clean the URI from any prefix
     * e.g. rdfs:isA -> isA
     */
    val pattern = "^.+#".r
    val urlPattern = "[^\\/]+$".r
    val out = pattern.replaceFirstIn(uri.toString, "")
    return urlPattern.findFirstIn(out).getOrElse("")
  }

  def cleanLabel(label: RDFNode): String = {
    /**
     * clean the LABEL removing the language
     * e.g. creator@en -> creator
     */
    val languageRegex = "@\\w{2}$".r
    return languageRegex.replaceFirstIn(label.toString, "")
  }

  def filterLanguage(label: RDFNode, language: String): Boolean = {
    /**
     * check if a LABEL is in the desired LANGUAGE (if specified); if not
     * specified it is considered true by default
     */
    if (label == null)
      return true
    val languageRegex = "@\\w{2}$".r
    val lang = languageRegex.findFirstIn(label.toString).getOrElse("@")
    return lang == "@" + language 
  }

  def apply(node: QuerySolution, variable: String, language: String): String = {
    /**
     * translate a result into text: translate a NODE into a string taking
     * either the (cleaned) label, if available, or trying a lexicalization from
     * the URI 
     */
    val labelQuery = s"""SELECT DISTINCT ?label WHERE {
                        |  <${node.get("?" + variable)}>  rdfs:label  ?label
                        |}""".stripMargin
    val labels = KG(labelQuery).toArray.filter(
      s => filterLanguage(s.get("?label"), language))
    val label = labels.head.get("?label")
    var out = label match {
      case null => cleanLabel(label)
      case _    => cleanUri(node.get("?" + variable))
    }
    out = "\\([^)]+\\)".r.replaceAllIn(out, " ")         // remove parenthesis
    out = "\"".r.replaceAllIn(out, " ")                  // remove quotes
    out = "\\\\".r.replaceAllIn(out, " ")                // remove slashes
    out = ":".r.replaceAllIn(out, " ")                   // remove double points
    out = "_".r.replaceAllIn(out, " ")                   // remove underscores
    out = "([a-z])([A-Z])".r.replaceAllIn(out, "$1 $2")  // camel case
    out = "\\s+".r.replaceAllIn(out, " ")                // remove multiple spaces
    return out.trim                                      // remove superflous spaces
  }
}


object NEE {

  def getWithLanguage(variable: String, label: String, lang: String): String =
    /**
     * get a piece of query to retrieve a LABEL in any desired LANGuage
     */
    s"""  {
       |    $variable ?r "$label"@$lang.
       |  } UNION {
       |    $variable ?r "$label".
       |  }
       |""".stripMargin


  def getEntities(subTree: Sentence): Array[QuerySolution] = {
    /**
     * get the array of entities for a SUBTREE
     */
    
    // exclude the string if has not a valid syntax construction
    if (! subTree.isValidTree)
      return Array[QuerySolution]()

    // other strings are analyzed in order to search for combinations of upper/lower cases
    if (printLog()) println("checking: \"" + subTree.sentence + "\"")
    val query = "SELECT DISTINCT ?topic WHERE {\n" + 
      binaryPermutations(subTree.length).map(
        candidatePerm => {
          val forma: String = subTree.text.zipWithIndex.map(
            i => {
              if (candidatePerm(i._2)) i._1.capitalize
              else i._1
            }).mkString(" ")
          getWithLanguage("?topic", forma, subTree.lang)
        }).mkString(" UNION ") + "\n}"
    return KG(query).toArray
  }


  def apply(tree: Sentence): Array[RDFtranslation] = {
    /**
     * the main function: analyze the dependency TREE in order to find a topic
     * entity; the used algorithm is a sliding window that checks if the
     * sub-tree (rebuilt as a sentence appears as a label in the KG)
     * The returned Sentence is the deeper candidate found
     */
    var checked = List[Sentence]()
    var out: Array[RDFtranslation] = Array[RDFtranslation]()
    var outScore: Int = 0

    // use the sliding window
    for (windowSize <- (1 to Array[Int](tree.length - 1,
                                    getConfig("maximumWindow").toInt).min).reverse;
       i <- 0 to (tree.length - windowSize)) { // for each possible substring

      val candidate: Sentence = tree.getPortion((i, i + windowSize))
      if (! checked.exists(isSubStringOf(candidate, _)) &&
            (windowSize > 1 || (windowSize == 1 &&
                                 POS.openClassPOS(tree.pos(i))))) {
        val entities: Array[QuerySolution] = getEntities(candidate).toArray
        if (! entities.isEmpty) {
          checked = checked :+ candidate
          val candidateScore = getTreeMaxDepth(candidate, tree)
          if (outScore <= candidateScore) {
            out = entities.map(e => new RDFtranslation(candidate, e.get("?topic")))
            outScore = candidateScore
            if (printLog()) println(s" - candidates: ${out.length}")
          }
        }
      }
    }

    // if desperated, check for single lemmas
    if (checked.isEmpty) {
      for (i <- 0 until tree.length)
        if (POS.openClassPOS(tree.pos(i))) {
          val candidate = tree.getPortion(i, i + 1)
          val entities = getEntities(candidate).toArray
          if (! entities.isEmpty) {
            if (printLog()) println(s" - candidates: ${entities.length}")
            return entities.map(e => new RDFtranslation(candidate, e.get("?topic")))
          }
        }
      return Array[RDFtranslation]()
    }
    return out
  }

}
