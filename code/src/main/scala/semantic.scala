package semantics


import org.apache.jena.query.QuerySolution
import org.apache.jena.rdf.model.RDFNode

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._


import nlp._
import main.utils._
import main.constants._



object DUDES {

  abstract class MainDUDES(val sentence: Sentence,
                           val o: Option[RDFNode] = None,
                           val r: Option[RDFNode] = None,
                           val c: Option[RDFNode] = None,
                           val score: Double = 1.0) {

    def getObjectDUDES(): String = {
      if (o.isDefined) return s"<${o.get}>"
      return s"?var_${getTreeRoot(sentence)}"
    }
    def getRelationDUDES(): String = {
      if (r.isDefined) return s"<${r.get}>"
      return s"?var_${getTreeRoot(sentence)}_relation"
    } 
    def getClassDUDES(): String = {
      if (c.isDefined) return s"<${c.get}>"
      return s"?var_${getTreeRoot(sentence)}_class"
    }

    def toRDF(prev: MainDUDES): String = getObjectDUDES()
    def toRDF(): String = getObjectDUDES()

    def apply(): RDFNode = o.get
  
    override def toString(): String = sentence.sentence
  }

  case class IncognitaDUDES(override val sentence: Sentence)
      extends MainDUDES(sentence)

  case class VariableDUDES(override val sentence: Sentence, variable: RDFNode)
      extends MainDUDES(sentence, o = Some(variable))

  case class RelationDUDES(override val sentence: Sentence,
                           rel: RDFNode,
                           override val score: Double = 1.0)
      extends MainDUDES(sentence, r = Some(rel), score = score) {

    override def toRDF(prev: MainDUDES): String =
      s"${prev.getObjectDUDES()}  ${getRelationDUDES()}  ${getObjectDUDES()} ."
  }

  case class ObjectDUDES(override val sentence: Sentence,
                         obj: RDFNode,
                         override val score: Double = 1.0)
      extends MainDUDES(sentence, o = Some(obj), score = score) {
    override def toRDF(prev: MainDUDES): String =
      s"${prev.getObjectDUDES()}  ${getRelationDUDES()}  ${getObjectDUDES()} ."
  }

  case class ClassDUDES(override val sentence: Sentence,
                        cls: RDFNode,
                        override val score: Double = 1.0)
      extends MainDUDES(sentence, c = Some(cls), score = score) {
    override def toRDF(prev: MainDUDES): String =
      s"""${prev.getObjectDUDES()}  ${getRelationDUDES()}  ${getObjectDUDES()} .
         |${getObjectDUDES()}  a  ${getClassDUDES()}""".stripMargin
  }

  class SolutionGraph(val graph: Graph[MainDUDES, DiEdge]) {

    // the score is signed in logaritmic scale: it is more substainable in long sentences
    // and the direction is the same: higher is the score, more probable is the match
    val score: Double = graph.nodes.map(n => math.log(n.value.score)).sum

    def getNode(dudes: DUDES.MainDUDES): Option[graph.NodeT] =
      Some(graph.nodes.toList.filter(node => node.value == dudes).head)

    def getTopic(): graph.NodeT =
      /**
       * get the topic node of the graph
       */
      graph.nodes.filter(n => n.value match {
                           case n: VariableDUDES => true
                           case _ => false
                         }).toList.head


    def getDistance(node: graph.NodeT): Int =
      /**
       * get the distance of a node from the topic node
       */
      node.value match {
        case n: VariableDUDES => 0
        case _ => {
          val path = getTopic.shortestPathTo(node)
          if (path.isDefined) path.get.length
          else                -1
        }
      }

    def getNodes(): List[graph.NodeT] =
      /**
       * get nodes from the closer to the more distant from the topic node
       */
      graph.nodes.toList.sortBy(getDistance)


    def addDUDES(relation: DiEdge[MainDUDES]): Option[SolutionGraph] = {
      /**
       * create a copy of this SolutionGraph with the addition of a relation (and the new RELATION)
       */
      val newNodes = List(relation._1, relation._2).filterNot(dudes => getNode(dudes).isDefined)
      if (newNodes.length == 1) {
        val node = newNodes.head
        val oldNodes: Array[MainDUDES] = graph.nodes.map(n => n.value).toArray
        val oldEdges: Array[DiEdge[MainDUDES]] = graph.edges.map(e => e._1.value ~> e._2.value).toArray
        val newGraph: Graph[MainDUDES, DiEdge] = Graph.from(oldNodes :+ node,
                                                            oldEdges :+ relation)
        return Some(new SolutionGraph(newGraph))
      }
      return None
    }
  }
}


class SentenceNode(val sentence: Sentence, val rdf: Array[RDFNode] = Array[RDFNode]()) {
  def destroy(): Array[SentenceNode] = {
    /**
     * transform this translation into a list of single meaningless SentenceNodes
     */
    val sentences = sentence.id.map(i => sentence.get(Array(i)))
    return sentences.map(s => new SentenceNode(s))
  }     

  override def toString(): String = sentence.sentence
}




object QASystem {

  def expandGraph(node: RDFNode, out: Boolean): Array[QuerySolution] = {
    /**
     * expand the graph starting from a NODE in either directions in or OUT
     */
    val queryDirection = out match {
      case true => s"<$node>  ?relation  ?object"
      case _ => s"?object  ?relation  <$node>"
    }
    val query = s"""SELECT ?relation  ?object  ?class
                   |WHERE {
                   |  $queryDirection .
                   |  OPTIONAL { ?object  a  ?class . }
                   |}""".stripMargin
    return KG(query).toArray
  }


  def exploreTreeUp(tree: Sentence, topic: Sentence): Array[Sentence] = {
    /**
     * get a list of candidates for the next step, starting from a TOPIC going up in the TREE
     */
    val headQuestion = getTreeRoot(tree).toInt - 1
    val headTopic = getTreeRoot(topic)
    val headTopicPosition = headTopic.toInt - 1 

    def getNext(position: Int, acc: Array[Sentence]): Array[Sentence] = {
      val head = tree.dep(position).toInt - 1
      if (head == headQuestion) return acc :+ tree.getPortion((head, headTopicPosition))
      getNext(head, acc :+ tree.getPortion((head, headTopicPosition)))
    }
    return getNext(headTopicPosition, Array[Sentence]())
  }

  def exploreTreeDown(tree: Sentence, topic: Sentence): Array[Sentence] = ???

  def exploreTree(tree: Sentence, topic: Sentence): Array[Sentence] =
    exploreTreeUp(tree, topic)


  def solve(explored: List[DUDES.SolutionGraph]): DUDES.SolutionGraph = {
    val maxScore: Double = explored.map(sg => sg.score).max
    val toProcess: List[DUDES.SolutionGraph] = explored.filter(sg => sg.score == maxScore) 
    return toProcess.head
  }


  def apply(question: String): String = {
    val tree: Sentence = Parser(question)
    val sentenceGraphs: List[DUDES.SolutionGraph] = NEE(tree)
    return "42"
  }

}


object Match {

  def isInCandidate(candidate: Sentence, topic: Sentence): Boolean =
    isSubStringOf(topic, candidate)

  def getNextDUDES(candidate: DUDES.SolutionGraph, question: Sentence): Option[DUDES.MainDUDES] = {
    /**
     * get the node closer to the topic with something to explore
     */
    val remainingSentences: Array[Sentence] = {
      val sentenceIntoGraph: Set[String] = candidate.graph.nodes.map(
        n => n.value.sentence.id).flatten.toSet
      val sentenceToParse: Sentence = question.get(question.id.filterNot(
                                                     i => sentenceIntoGraph.contains(i)))
      splitIntoSubtrees(sentenceToParse)
        .filter(sent => sent.pos.exists(POS.openClassPOS)) }

    candidate.getNodes.map(n => n.value).foreach(
      node => {
        // sentence -[depends by]-> node
        remainingSentences.foreach(s => {
                                     val h = getTreeRoot(s)
                                     val d = s.get(h).dep.head
                                     if (node.sentence.id.contains(d)) {
                                       return Some(node)
                                     }
                                   })
        // node -[depends by]-> sentence
        val head = getTreeRoot(node.sentence)
        val dependsBy = node.sentence.get(head).dep.head
        remainingSentences.foreach(s => {
                                     if (s.id.contains(dependsBy)) {
                                       return Some(node)
                                     }
                                   })
      })
    return None
  }

  def perfectMatch(candidate: DUDES.SolutionGraph, question: Sentence): DUDES.SolutionGraph = {


    return candidate
  }
}
