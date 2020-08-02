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

  def cleanText(text: String): String = {
    ( (out: String) => "\\([^)]+\\)".r.replaceAllIn(out, " ")).andThen(
      (out: String) => "\"".r.replaceAllIn(out, " ")).andThen(
      (out: String) => "\\\\".r.replaceAllIn(out, " ")).andThen(
      (out: String) => ":".r.replaceAllIn(out, " ")).andThen(
      (out: String) => "_".r.replaceAllIn(out, " ")).andThen(
      (out: String) => "([a-z])([A-Z])".r.replaceAllIn(out, "$1 $2")).andThen(
      (out: String) => "\\s+".r.replaceAllIn(out, " "))
      (text).trim
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

  def apply(node: RDFNode, language: String): String = {
    /**
     * translate a result into text: translate a NODE into a string taking
     * either the (cleaned) label, if available, or trying a lexicalization from
     * the URI 
     */
     val labelQuery = s"""SELECT DISTINCT ?label WHERE {
                        |  <$node>  rdfs:label  ?label
                        |}""".stripMargin
    val labels = KG(labelQuery).toArray.filter(
      s => filterLanguage(s.get("?label"), language))
    if (labels.isEmpty) return cleanText(cleanUri(node))
    val label = labels.head.get("?label")
    val out = label match {
      case null => cleanLabel(label)
      case _    => cleanUri(node)
    }
    return cleanText(out)
  }
  def apply(node: QuerySolution, variable: String, language: String): String = apply(node.get(s"?$variable"), language)
}


object NEE {

  class NLPGraph(val graph: Graph[SentenceNode, DiEdge]) {
    def getNodes(): List[SentenceNode] = graph.nodes.toList.map(_.toOuter)

    override def toString(): String = {
      val nodes = getNodes()
      val nodesSorted = nodes.sortBy(n => n.sentence.id.head.toInt)
      return nodesSorted.map(_.toString).mkString(" ")
    }

    def getHead(startNode: graph.NodeT = graph.nodes.head): graph.NodeT = {
      val nextNode = startNode.edges.head._1
      if (startNode == nextNode) return nextNode
      return getHead(nextNode)
    }

    def getNode(s: Sentence): List[graph.NodeT] = {
      graph.nodes.toList.filter(n => n.toOuter.sentence == s)
    }
  }

  def sentenceToGraph(sentences: Map[Sentence, Array[RDFNode]]): Array[NLPGraph] = {
    /**
     * transform the sentence into a graph
     */
    val nodesList: List[List[SentenceNode]] = sentences.map(
        kv => {
          val sentence = kv._1
          val candidates = kv._2
          candidates.map(c => new RDFtranslation(sentence, c)).toList
        }).toList

    def createGraph(nodes: Array[SentenceNode]): NLPGraph = {
      val getNode = (id: String) => nodes.filter(n => n.sentence.id.contains(id))
      val headOf = (s: Sentence) => {
        val rootId = getTreeRoot(s)
        val rootIndex = s.id.indexOf(rootId)
        getNode(s.dep(rootIndex))
      }

      def getEdges(toProcess: Array[SentenceNode], acc: Array[DiEdge[SentenceNode]] = Array[DiEdge[SentenceNode]]()): List[DiEdge[SentenceNode]] = {
        if (toProcess.isEmpty) return acc.toList
        val current = toProcess.head
        val head = headOf(current.sentence)
        if (head.isEmpty) return getEdges(toProcess.tail, acc)
        return getEdges(toProcess.tail, acc :+ head.head~>current)
      }

      return new NLPGraph(Graph.from(nodes, getEdges(nodes)))
    }
    def cartesianProduct[T](list: Seq[Seq[T]]): Seq[Seq[T]] = list match {
      case Nil => Seq(Nil)
      case h :: t => for(xh <- h; xt <- cartesianProduct(t)) yield xh +: xt
    }
    return cartesianProduct(nodesList).map(n => createGraph(n.toArray)).toArray
  }


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


  def slidingWindow(tree: Sentence, maxSize: Int = getConfig("maximumWindow").toInt): Map[Sentence, Array[RDFNode]] = {
    /**
     * use a sliding window to get a set of candiadtes for a TREE; window size
     * can be set with the parameter MAXSIZE
     */
    var sentenceTree = scala.collection.mutable.Map[Sentence, Array[RDFNode]]()
    for (windowSize <- (1 to math.min(tree.length - 1, maxSize)).reverse;
       i <- 0 to (tree.length - windowSize)) {
      val candidate: Sentence = tree.getPortion((i, i + windowSize))
      if (! sentenceTree.keySet.exists(isSubStringOf(candidate, _)))
        if (windowSize > 1 || (windowSize == 1 && POS.openClassPOS(tree.pos(i)))) {
          val entities: Array[QuerySolution] = getEntities(candidate)
          if (! entities.isEmpty) {
            val topics = entities.map(e => e.get("?topic")).toArray
            if (printLog())
              println(s"${candidate.sentence}: ${topics.length}")
            sentenceTree += (candidate -> topics)
          }
        } else if (windowSize == 1) sentenceTree += (candidate -> Array[RDFNode]())
    }
    if (printLog()) {
      println("\n")
      sentenceTree.foreach(i =>
        if (i._2.isEmpty) println(s"${i._1.sentence}")
        else println(s"${i._1.sentence}: \n" + i._2.map(i => s" - $i").mkString("\n")))
    }
    return sentenceTree.toMap
  }

  def apply(tree: Sentence): Array[NLPGraph] = {
    /**
     * return the TREE annotated with some semantics nodes
     */
    val sentenceTree = slidingWindow(tree)
    val graphs = sentenceToGraph(sentenceTree)
    return graphs
  }

}
