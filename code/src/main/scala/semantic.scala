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
                           val c: Option[RDFNode] = None) {

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

  class IncognitaDUDES(sentence: Sentence)
      extends MainDUDES(sentence)

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

  class SolutionGraph(val graph: Graph[DUDES.MainDUDES, DiEdge],
                      val score: Double,
                      val toParse: Array[Sentence] = Array[Sentence]())
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

  def apply(question: String): String = {
    val tree: Sentence = Parser(question)
    NEE(tree)
    // val sentenceGraph: NEE.NLPGraph = NEE(tree)
    return "42"
  }

}


object Match {

  def isInCandidate(candidate: Sentence, topic: Sentence): Boolean =
    isSubStringOf(topic, candidate)

  def apply(candidates: Array[Sentence], topic: Sentence): Unit = {
  }

}
