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
                           val o: Some[RDFNode] = null,
                           val r: Some[RDFNode] = null,
                           val c: Some[RDFNode] = null) {

    def getObjectDUDES(): String = {
      if (o.isDefined) return s"<$o>"
      return s"?var_${getTreeRoot(sentence)}"
    }
    def getRelationDUDES(): String = {
      if (r.isDefined) return s"<$r>"
      return s"?var_${getTreeRoot(sentence)}_relation"
    } 
    def getClassDUDES(): String = {
      if (c.isDefined) return s"<$c>"
      return s"?var_${getTreeRoot(sentence)}_class"
    }

    def toRDF(prev: MainDUDES): String = getObjectDUDES()
    def toRDF(): String = getObjectDUDES()

    def apply(): Some[RDFNode] = o
  
    override def toString(): String = sentence.sentence
  }

  class IncognitaDUDES(sentence: Sentence) extends MainDUDES(sentence)

  class VariableDUDES(sentence: Sentence, variable: RDFNode)
      extends MainDUDES(sentence, o = Some(variable))

  class RelationDUDES(sentence: Sentence, rel: RDFNode)
      extends MainDUDES(sentence, r = Some(rel)) {

    override def toRDF(prev: MainDUDES): String =
      s"${prev.getObjectDUDES()}  ${getRelationDUDES()}  ${getObjectDUDES()} ."
  }

  class ObjectDUDES(sentence: Sentence, obj: RDFNode)
      extends MainDUDES(sentence, o = Some(obj)) {
    override def toRDF(prev: MainDUDES): String =
      s"${prev.getObjectDUDES()}  ${getRelationDUDES()}  ${getObjectDUDES()} ."
  }

  class ClassDUDES(sentence: Sentence, cls: RDFNode)
      extends MainDUDES(sentence, c = Some(cls)) {
    override def toRDF(prev: MainDUDES): String =
      s"""${prev.getObjectDUDES()}  ${getRelationDUDES()}  ${getObjectDUDES()} .
         |${getObjectDUDES()}  a  ${getClassDUDES()}""".stripMargin
  }

}


class SentenceNode(val sentence: Sentence, val rdf: Option[RDFNode] = None) {
  def destroy(): Array[SentenceNode] = {
    /**
     * transform this translation into a list of single meaningless SentenceNodes
     */
    val sentences = sentence.id.map(i => sentence.get(Array(i)))
    return sentences.map(s => new SentenceNode(s))
  }     

  override def toString(): String = rdf.getOrElse(sentence.sentence).toString
}

class SolutionGraph(graph: Graph[SentenceNode, DiEdge], val score: Double)
    extends NEE.NLPGraph(graph)


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
  def expandGraph(topic: SentenceNode, out: Boolean): Array[QuerySolution] =
    if (topic.rdf.isDefined) expandGraph(topic.rdf.get, out)
    else Array[QuerySolution]()


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

  def filterTopics(graphs: Array[NEE.NLPGraph],
                 candidates: Map[Sentence, Array[RDFNode]]): Array[SolutionGraph] = {
    /**
     * filter the GRAPHS according to CANDIDATES and transform them in SolutionGraphs
     */
    val stepOrder: List[Sentence] = graphs.head.getRDFTranslations.map(n => n.toOuter.sentence)

    def filterNextStep(node: RDFNode, nextNode: Seq[RDFNode], out: Boolean): Seq[RDFNode] = {
      val nextStep = expandGraph(node, out)
      val nodes = (nextStep.map(n => n.get("?relation")).toSet ++
                     nextStep.map(n => n.get("?object")).toSet ++
                     nextStep.map(n => n.get("?class")).toSet)
      return nextNode.filter(n => nodes.contains(n))
    }
    def addRemainingNodes(g: SolutionGraph): SolutionGraph = {
      return g
    }
    def initialize(input: Array[NEE.NLPGraph],
                 acc: Array[SolutionGraph] = Array[SolutionGraph](),
                 step: Int = 0): Array[SolutionGraph] = {

      if (step >= stepOrder.size - 1) return acc.map(addRemainingNodes)

      candidates(stepOrder(step)).map(
        candidate => {  // : RDFNode
          val nextStepsOut = filterNextStep(candidate, candidates(stepOrder(step + 1)), true)
          true
        })
      return initialize(input, acc, step + 1)
    }
    return initialize(graphs)
  }

  def apply(question: String): String = {
    val tree: Sentence = Parser(question)
    val (topics: Map[Sentence, Array[RDFNode]], graphs: Array[NEE.NLPGraph]) = NEE(tree)
    val topicsFiltered: Array[SolutionGraph] = filterTopics(graphs, topics)
    return "42"
  }

}


object Match {

  def isInCandidate(candidate: Sentence, topic: Sentence): Boolean =
    isSubStringOf(topic, candidate)

  def apply(candidates: Array[Sentence], topic: Sentence): Unit = {
  }

}
