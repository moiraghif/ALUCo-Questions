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
  /*
  class NLPGraph(val graph: Graph[SentenceNode, DiEdge]) {
    // A ~> B  =>  A dependsOn B
    def getNodes(): Set[SentenceNode] = graph.nodes.toList.map(n => n.value).toSet
    def getValue(node: graph.NodeT): SentenceNode = node.value

    def getHead(startNode: graph.NodeT = graph.nodes.head): graph.NodeT = {
      val nextNodes = startNode.edges.filter(n => n._2 == startNode).map(_._1)
      if (nextNodes.isEmpty) return startNode
      return getHead(nextNodes.head)
    }

    def getNode(node: SentenceNode): Option[graph.NodeT] =
      Option(graph.nodes.toList.filter(n => n.value == node).head)
    def getNode(s: Sentence): Option[graph.NodeT] =
      Option(graph.nodes.toList.filter(n => n.value.sentence == s).head)

    def getRDFTranslations(): List[graph.NodeT] = {
      val nodes = graph.nodes.toList.filter(n => n.value.rdf.isDefined)
      val head = getHead()
      return nodes.sortBy(n => head.shortestPathTo(n).get.length).reverse
    }

    def areConsecutive(node1: graph.NodeT, node2: graph.NodeT): Boolean =
      node1.edges.toSet.contains(node1~>node2) || node1.edges.toSet.contains(node2~>node1)
    def areConsecutive(sent1: Sentence, sent2: Sentence): Boolean = {
      val node1 = getNode(sent1)
      val node2 = getNode(sent2)
      if (node1.isDefined && node2.isDefined) return areConsecutive(node1.get, node2.get)
      return false
    }

    def contains(node: graph.NodeT): Boolean = contains(node.value)
    def contains(node: SentenceNode): Boolean = getNodes().contains(node)
    def contains(sentence: Sentence): Boolean = getNode(sentence).isDefined
    def contains(node: RDFNode): Boolean = getNodes().exists(n => n.rdf.contains(node))

    override def toString(): String = {
      val nodes = getNodes().toArray
      val nodesSorted = nodes.sortBy(n => n.sentence.id.head.toInt)
      return nodesSorted.map(_.toString).mkString(" ")
    }
  }

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

  def sentenceToGraph(sentences: Map[Sentence, Array[RDFNode]]): NLPGraph = {
    val nodesList: List[SentenceNode] = sentences.map(
        kv => {
          val sentence = kv._1
          val candidates = kv._2
          new SentenceNode(sentence, candidates)
        }).toList
    return createGraph(nodesList.toArray)
  }
   */

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
      if (! sentenceTree.keySet.exists(isSubStringOf(candidate, _)) &&
            (windowSize > 1 || (windowSize == 1 && POS.openClassPOS(tree.pos(i))))) {
        val entities: Array[QuerySolution] = getEntities(candidate)
        if (! entities.isEmpty) {
          val topics = entities.map(e => e.get("?topic")).toArray
          sentenceTree += (candidate -> topics)
        }
      }
    }
    if (printLog()) {
      println("")
      sentenceTree.foreach(i =>
        println(s"${i._1.sentence}: \n" + i._2.map(i => s" - $i").mkString("\n") + "\n"))
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

    def filter(nodes: Map[List[DUDES.MainDUDES], List[DiEdge[DUDES.MainDUDES]]],
             i: Int = 0): Map[List[DUDES.MainDUDES], List[DiEdge[DUDES.MainDUDES]]] = {
      if (i >= parsingOrder.length)  // end of the list
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
        val newCandidates: List[RDFNode] = candidates(nextSent).toList
        val nextSteps: Array[QuerySolution] = QASystem.expandGraph(oldDudes(), out)
        println(oldDudes.toRDF)
        val r = nextSteps.map(s => s.get("?relation"))
        val o = nextSteps.map(s => s.get("?object"))
        val c = nextSteps.map(s => s.get("?class"))
        newCandidates.map(rdf => {
          if (r.contains(rdf)) {
            val newDudes = new DUDES.RelationDUDES(nextSent, rdf)
            if (out) (Some(newDudes), Some(oldDudes ~> newDudes))
            else     (Some(newDudes), Some(newDudes ~> oldDudes))
          } else if (o.contains(rdf)) {
            val newDudes = new DUDES.ObjectDUDES(nextSent, rdf)
            if (out) (Some(newDudes), Some(oldDudes ~> newDudes))
            else     (Some(newDudes), Some(newDudes ~> oldDudes))
          } else if (c.contains(rdf)) {
            val newDudes = new DUDES.ClassDUDES(nextSent, rdf)
            if (out) (Some(newDudes), Some(oldDudes ~> newDudes))
            else     (Some(newDudes), Some(newDudes ~> oldDudes))
          } else (None, None)
        }).filter(kv => kv._1.isDefined)
          .map(kv => (oldDudesList :+ kv._1.get, oldEdges :+ kv._2.get))
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
      new DUDES.VariableDUDES(parsingOrder.head, rdf))
      .map(d => (List(d), List[DiEdge[DUDES.MainDUDES]]()))
      .toMap[List[DUDES.MainDUDES], List[DiEdge[DUDES.MainDUDES]]]

    val dudes = filter(topicDUDES)

    if (printLog()) {
      println("Disambiguation results:")
      for (kv <- dudes) {
        val dude = kv._1
        val links = kv._2
        println(s"- $dude : $links")
      }
    }

    return dudes.map(
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
  }


  def apply(tree: Sentence): List[DUDES.SolutionGraph] = {
    /**
     * return a DUDES representation of the sentence
     */
    return disambiguate(tree, slidingWindow(tree))
  }

}
