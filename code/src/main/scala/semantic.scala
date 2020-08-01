package semantics


import org.apache.jena.query.QuerySolution
import org.apache.jena.rdf.model.RDFNode

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._


import nlp._
import main.utils._
import main.constants._



class SentenceNode(val sentence: Sentence)
class RDFtranslation(sentence: Sentence, val rdf: RDFNode) extends SentenceNode(sentence)



object QASystem {

  class SolutionGraph(val graph: Graph[SentenceNode, DiEdge], val score: Double, val remainingSentence: Sentence) {
    def apply(): Array[SolutionGraph] = {
      val subTrees = splitIntoSubtrees(remainingSentence) 
      return Array[SolutionGraph]()
    }
  }

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
  def expandGraph(topic: RDFtranslation, out: Boolean): Array[QuerySolution] =
    expandGraph(topic.rdf, out)


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


  def apply(question_test: String): String = {
    val question = "Who is the director of Titanic with Leonardo DiCaprio ?"
    val tree: Sentence = Parser(question)
    val topics: Array[RDFtranslation] = NEE(tree)
    val graphs: Array[SolutionGraph] = topics.map(
      t => {
        val graph: Graph[SentenceNode, DiEdge] = Graph(t)
        val sentenceWithoutTopic = tree.remove(t.sentence)
        new SolutionGraph(graph, 1.0, sentenceWithoutTopic)
      })
    return "42"
  }

}


object Match {

  def isInCandidate(candidate: Sentence, topic: Sentence): Boolean =
    isSubStringOf(topic, candidate)

  def apply(candidates: Array[Sentence], topic: Sentence): Unit = {
  }

}
