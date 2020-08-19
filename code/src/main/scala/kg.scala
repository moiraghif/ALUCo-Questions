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

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._


import main.constants._
import main.utils._
import nlp.{
  Parser,
  POS,
  Sentence }
import semantics.DUDES


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


  def queryAsk(query: String): Boolean = {
    Thread.sleep(getLatency().toLong)
    try queryKG(query).execAsk()
    catch {
      case e: org.apache.jena.sparql.resultset.ResultSetException => return false
    }
  }

  def querySelect(query: String): List[QuerySolution] = { 
    /**
     * execute a select QUERY from the standard database; it returns an iterator
     * of possible solutions
     * this function is polite: it respects the latency setted in config.json
     */
    Thread.sleep(getLatency().toLong)
    try queryKG(query).execSelect().asScala.toList
    catch {
      case e: org.apache.jena.sparql.resultset.ResultSetException => return List[QuerySolution]()
    }
  }
  def apply(query: String): List[QuerySolution] = querySelect(query)

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

  def cleanText(text: String): String = {
    var out = text
    out = "\\([^)]+\\)".r.replaceAllIn(out, " ")
    out = "\"".r.replaceAllIn(out, " ")
    out = "\\\\".r.replaceAllIn(out, " ")
    out = ":".r.replaceAllIn(out, " ")
    out = "_".r.replaceAllIn(out, " ")
    out = "([a-z])([A-Z])".r.replaceAllIn(out, "$1 $2")
    out = "\\s+".r.replaceAllIn(out, " ")
    return out.trim
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

  def apply(queries: List[QuerySolution], language: String, variable: String): Map[RDFNode, List[String]] = {
    val nodes: List[RDFNode] = queries.map(q => q.get(s"?$variable")).toSet.toList
    val validQueries = queries.filter(q => filterLanguage(q.get(s"?${variable}_label"), language))
    val lexicalizations: Map[RDFNode, List[RDFNode]] = nodes
      .map(n => (n, validQueries.filter(q => q.get(s"?$variable") == n)
                                .map(q => q.get(s"?${variable}_label"))))
      .toMap
    val out = lexicalizations
      .map(kv => (kv._1, cleanUri(kv._1) +: kv._2.filter(e => e != null).map(cleanLabel)))
      .map(kv => (kv._1, kv._2.map(cleanText)))
    return out
  }

  def apply(node: DUDES.MainDUDES, language: String): String = {
    /**
     * translate a result into text: translate a NODE into a string taking
     * either the (cleaned) label, if available, or trying a lexicalization from
     * the URI 
     */
    val labelQuery = s"""SELECT DISTINCT ?label WHERE {
                        |  $node  rdfs:label  ?label
                        |}""".stripMargin
    val label = KG(labelQuery)
      .filter(q => filterLanguage(q.get("?label"), language))
      .map(q => q.get("?label"))
      .filter(_ != null)
      .map(cleanLabel)
      .headOption.getOrElse(cleanUri(node match {
                                       case node: DUDES.RelationDUDES => node.r.get
                                       case node: DUDES.ClassDUDES => node.c.get
                                       case node: DUDES.ClassIncognitaDUDES => node.c.get
                                       case _ => node()
                                     }))
    return cleanText(label)
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


  def getEntities(subTree: Sentence): List[QuerySolution] = {
    /**
     * get the array of entities for a SUBTREE
     */
    
    // exclude the string if has not a valid syntax construction
    if (! subTree.isValidTree)
      return List[QuerySolution]()

    // other strings are analyzed in order to search for combinations of upper/lower cases
    if (printLog()) println(s"checking: $subTree")
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
    return KG(query)
  }


  def slidingWindow(tree: Sentence, maxSize: Int = getConfig("maximumWindow").toInt):
      Map[Sentence, Array[RDFNode]] = {
    /**
     * use a sliding window to get a set of candiadtes for a TREE; window size
     * can be set with the parameter MAXSIZE
     */
    var sentenceTree = scala.collection.mutable.Map[Sentence, Array[RDFNode]]()
    for (windowSize <- (1 to math.min(tree.length - 1, maxSize)).reverse;
       i <- 0 to (tree.length - windowSize)) {
      val candidate: Sentence = tree.getPortion((i, i + windowSize))
      if (! sentenceTree.keySet.exists(isSubStringOf(candidate, _)) &&
            (windowSize > 1 || (windowSize == 1 && POS.openClassPOS(tree.pos(i))))) {
        val entities: List[QuerySolution] = getEntities(candidate)
        if (! entities.isEmpty) {
          val topics = entities.map(e => e.get("?topic")).toArray
          sentenceTree += (candidate -> topics)
        }
      }
    }
    if (printLog()) {
      println("")
      sentenceTree.foreach(i =>
        println(s"${i._1}: \n" + i._2.map(i => s" - $i").mkString("\n") + "\n"))
    }
    return sentenceTree.toMap
  }

  def disambiguate(sentence: Sentence, candidates: Map[Sentence, Array[RDFNode]]):
      List[DUDES.SolutionGraph] = {
    /**
     * returns a SolutionGraph disambiguation of the sentence
     */
    val parsingOrder: List[Sentence] = candidates
      .keySet.toList
      .sortBy(s => getTreeRoot(s).toInt )
      .reverse

    def areConsecutive(sent1: Sentence, sent2: Sentence): Boolean = {
      val check = (s1: Sentence, s2: Sentence)
      => !s2.dep.intersect(s1.id).isEmpty
      return check(sent1, sent2) || check(sent2, sent1)
    }

    def filter(nodes: Map[List[DUDES.MainDUDES], List[DiEdge[DUDES.MainDUDES]]], i: Int = 0):
        Map[List[DUDES.MainDUDES], List[DiEdge[DUDES.MainDUDES]]] = {
      if (i >= parsingOrder.length - 1)  // end of the list
        return nodes
      val sent = parsingOrder(i)
      val nextSent = parsingOrder(i + 1)
      if (! areConsecutive(sent, nextSent)) // non-consecutive nodes
        return nodes

      val getNexts = (oldDudesList: List[DUDES.MainDUDES],
                      oldEdges: List[DiEdge[DUDES.MainDUDES]],
                      out: Boolean) => {
        /**
         * this routine gives you new dudes (with relations) if they are linkable
         * to a new candidate (from newCandidates)
         */
        val oldDudes = oldDudesList.last
        if (! oldDudes.o.isDefined) return nodes
        val newCandidates: List[RDFNode] = candidates(nextSent).toList
        val nextSteps: List[QuerySolution] = QASystem.expandGraph(oldDudes(), out)
        val r = nextSteps.map(s => s.get("?relation"))
        val o = nextSteps.map(s => s.get("?object"))
        val c = nextSteps.map(s => s.get("?class"))
        newCandidates.map(rdf => {
                            if (r.contains(rdf)) {
                              val newDudes = new DUDES.RelationDUDES(nextSent, rdf, i + 1)
                              if (out) Some(newDudes, oldDudes ~> newDudes)
                              else     Some(newDudes, newDudes ~> oldDudes)
                            } else if (o.contains(rdf)) {
                              val newDudes = new DUDES.ObjectDUDES(nextSent, rdf, i + 1)
                              if (out) Some(newDudes, oldDudes ~> newDudes)
                              else     Some(newDudes, newDudes ~> oldDudes)
                            } else if (c.contains(rdf)) {
                              val newDudes = new DUDES.ClassDUDES(nextSent, rdf, i + 1)
                              if (out) Some(newDudes, oldDudes ~> newDudes)
                              else     Some(newDudes, newDudes ~> oldDudes)
                            } else None
                          }).filter(kv => kv.isDefined)
          .map(kv => (oldDudesList :+ kv.get._1, oldEdges :+ kv.get._2))
          .toMap
      }

      val out = nodes.map(kv => {
                            val oldDudes = kv._1
                            val oldEdges = kv._2
                            getNexts(oldDudes, oldEdges, true) ++
                            getNexts(oldDudes, oldEdges, false)
                          })
        .toList.flatten.toMap
        .filter(kv => ! kv._2.isEmpty)

      if (out.isEmpty) return nodes
      return filter(out, i + 1)
    }
    val topicDUDES = candidates(parsingOrder.head).map(rdf =>
      new DUDES.ObjectDUDES(parsingOrder.head, rdf, 0))
      .map(d => (List(d), List[DiEdge[DUDES.MainDUDES]]()))
      .toMap[List[DUDES.MainDUDES], List[DiEdge[DUDES.MainDUDES]]]
    val dudes = filter(topicDUDES)

    val out = dudes.map(
      kv => {
        val dudes = kv._1
        val edges = kv._2
        val dudesSentences = dudes.map(d => d.sentence.id)
        val restOfTheSentence: Array[String] = (1 to sentence.length).toArray
          .map(i => i.toString)
          .filterNot(i => dudesSentences.contains(i))
        val graph = Graph.from(dudes, edges)
        new DUDES.SolutionGraph(graph)
      }).toList
    if(printLog()) {
      out.foreach(println)
      println("\n\n")
    }
    return out
  }

  def apply(tree: Sentence): List[DUDES.SolutionGraph] = {
    /**
     * return a DUDES representation of the sentence
     */
    return disambiguate(tree, slidingWindow(tree))
  }

}
