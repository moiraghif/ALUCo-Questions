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

  def getTopic(candidate: SolutionGraph, question: Sentence):
      Option[(MainDUDES, Sentence, Boolean)] = {
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

    candidate.getNodes().map(n => n.value).foreach(
      node => {
        // sentence -[depends by]-> node
        remainingSentences.foreach(s => {
                                     val h = getTreeRoot(s)
                                     val d = s.get(h).dep.head
                                     if (node.sentence.id.contains(d)) {
                                       return Some(node, s, false)
                                     }
                                   })
        // node -[depends by]-> sentence
        val head = getTreeRoot(node.sentence)
        val dependsBy = node.sentence.get(head).dep.head
        remainingSentences.foreach(s => {
                                     if (s.id.contains(dependsBy)) {
                                       return Some(node, s, true)
                                     }
                                   })
      })
    return None
  }

  abstract class MainDUDES(val sentence: Sentence,
                           val o: Option[RDFNode] = None,
                           val r: Option[RDFNode] = None,
                           val c: Option[RDFNode] = None,
                           val score: Double = 1.0,
                           val dist: Int = 0) {

    def getObjectDUDES(): String = {
      if (o.isDefined) return s"<${o.get}>"
      return s"?var_${getTreeRoot(sentence)}"
    }
    def getRelationDUDES(): String = {
      if (r.isDefined) return s"<${r.get}>"
      return s"?relation_${getTreeRoot(sentence)}"
    } 
    def getClassDUDES(): String = {
      if (c.isDefined) return s"<${c.get}>"
      return s"?class_${getTreeRoot(sentence)}"
    }

    def toRDF(prev: MainDUDES): String =
      s"${prev.getObjectDUDES()}  ${prev.getRelationDUDES()}  ${getObjectDUDES()} ."
    def toRDF(): String = getObjectDUDES()

    def apply(): RDFNode = o.get
  }

  case class IncognitaDUDES(override val sentence: Sentence,
                            override val dist: Int)
      extends MainDUDES(sentence,
                        dist = dist) {
    override def toRDF(prev: MainDUDES): String = prev.toRDF()
    override def toString(): String = s"?var_${getTreeRoot(sentence)}"
  }

  case class VariableDUDES(override val sentence: Sentence,
                           variable: RDFNode)
      extends MainDUDES(sentence, o = Some(variable),
                        dist = 0) {
    override def toString(): String = s"<${o.get}>"
  }

  case class RelationDUDES(override val sentence: Sentence,
                           rel: RDFNode,
                           override val dist: Int,
                           override val score: Double = 1.0)
      extends MainDUDES(sentence, r = Some(rel),
                        score = score, dist = dist) {

    override def toRDF(prev: MainDUDES): String =
      s"${prev.getObjectDUDES()}  ${getRelationDUDES()}  ${getObjectDUDES()} ."

    override def toString(): String = s"<${r.get}>"

  }

  case class ObjectDUDES(override val sentence: Sentence,
                         obj: RDFNode,
                         override val dist: Int,
                         override val score: Double = 1.0)
      extends MainDUDES(sentence, o = Some(obj),
                        score = score, dist = dist) {
    /* override def toRDF(next: MainDUDES): String =
      s"${getObjectDUDES()}  ${next.getRelationDUDES()}  ${next.getObjectDUDES()} ." */

    override def toString(): String = s"<${o.get}>"
  }

  case class ClassDUDES(override val sentence: Sentence,
                        cls: RDFNode,
                        override val dist: Int,
                        override val score: Double = 1.0)
      extends MainDUDES(sentence, c = Some(cls),
                        score = score, dist = dist) {
    override def toRDF(prev: MainDUDES): String =
      s"${prev.getObjectDUDES()}  ${prev.getRelationDUDES()}  ${getObjectDUDES()} .  " +
        s"${getObjectDUDES()}  a  ${getClassDUDES()} ."

    override def toString(): String = s"<${c.get}>"
  }

  class SolutionGraph(val graph: Graph[MainDUDES, DiEdge]) {

    // the score is signed in logaritmic scale: it is more substainable in long sentences
    // and the direction is the same: higher is the score, more probable is the match
    val score: Double = graph.nodes.map(n => math.log(n.value.score)).sum
    val length: Int = graph.nodes.length

    def getNode(dudes: MainDUDES): Option[graph.NodeT] = {
      val out = graph.nodes.toList.filter(node => node.value == dudes)
      if (out.isEmpty) return None
      return Some(out.head)
    }

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
      node.value.dist

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

    def getVariables(): List[String] = {
      val incognitaNodes: List[MainDUDES] = graph.nodes
        .map(n => n.value)
        .filter(n => n match {
                  case n: IncognitaDUDES => true
                  case _ => false
                }).toList
      if (incognitaNodes.isEmpty) return List[String]()
      return incognitaNodes.map(dudes => dudes.toRDF(
                                  getNode(dudes).get.edges.head._1.value
                                ))
    }

    def makeRDF(query: List[String]): String = {
      val queryText = query.map(triple => s"  $triple").mkString("\n")
      val incognitaNodes = getVariables()

      if (incognitaNodes.isEmpty)
        return s"""ASK WHERE {
                  |$queryText
                  |}""".stripMargin

      val incognita = incognitaNodes.mkString(" ")
      return s"""SELECT DISTINCT $incognita WHERE {
                |$queryText
                |}""".stripMargin
    }

    override def toString(): String = {
      val triples = graph.edges.toList
        .filter(e => e._2.value match {
                  case n: IncognitaDUDES => false
                  case _ => true
                })
        .map(e => e._2.value.toRDF(e._1.value))
      return makeRDF(triples) 
    }

  }
}


class SentenceNode(val sentence: Sentence, val rdf: Array[RDFNode] = Array[RDFNode]()) {
  override def toString(): String = sentence.toString
}



