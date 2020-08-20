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
      Option[(MainDUDES, Sentence)] = {
    /**
     * get the node closer to the topic with something to explore
     */

    val remainingSentences: Array[Sentence] = {
      /*
       * the sentence to explore is those that is not econded in the DUDES; it
       * is than splitted into sub-trees and checked if any has something to say
       * (or, in other words, an element whose POS is an open class)
       */
      val sentenceIntoGraph: Set[String] = candidate.graph.nodes.map(
        n => n.value.sentence.id).flatten.toSet
      val sentenceToParse: Sentence = question.get(question.id.filterNot(
                                                     i => sentenceIntoGraph.contains(i)))
      splitIntoSubtrees(sentenceToParse)
        .filter(s => POS(s))
    }

    candidate.getNodes().map(n => n.value).foreach(
      node => {

        // sentence -[depends by]-> node
        remainingSentences.foreach(s => {
                                     val h = getTreeRoot(s)
                                     val d = s.get(h).dep.head
                                     if (node.sentence.id.contains(d))
                                       return Some(node, s)
                                   })

        // node -[depends by]-> sentence
        val head = getTreeRoot(node.sentence)
        val dependsBy = node.sentence.get(head).dep.head
        remainingSentences.foreach(s => {
                                     if (s.id.contains(dependsBy))
                                       return Some(node, s)
                                   })
      })

    return None
  }


  abstract class MainDUDES(val sentence: Sentence,
                           val variable: Option[RDFNode] = None,
                           val score: Double = 1.0,
                           val dist: Int = 0) {

    def getVariable(): String =
      /**
       * get the value (constant or variable) of the DUDES
       */
      if (variable.isDefined)  s"<${variable.get}>"
      else                     s"?var_${getTreeRoot(sentence)}"

    override def toString(): String =
      getVariable()

    def apply(): RDFNode = variable.get
  }

  /*
   * a list of possible concrete classes that the DUDES system supports
   */


  case class IncognitaDUDES(override val sentence: Sentence,
                            override val dist: Int)
  /*
   * the variable that is retrieved in a SELECT query
   * e.g. WHO is the director of TITANIC ?
   */
      extends MainDUDES(sentence,
                        dist = dist)


  case class ObjectDUDES(override val sentence: Sentence,
                         v: RDFNode,
                         override val dist: Int,
                         override val score: Double = 1.0)
  /*
   * a known entity
   * e.g.  who is the director of TITANIC ?
   */
      extends MainDUDES(sentence, variable = Some(v),
                        score = score, dist = dist)


  case class RelationDUDES(override val sentence: Sentence,
                           v: RDFNode,
                           override val dist: Int,
                           override val score: Double = 1.0)
  /*
   * a known relation
   * e.g. who is the DIRECTOR of Titanic ?
   */
      extends MainDUDES(sentence, variable = Some(v),
                        score = score, dist = dist)


  case class ClassDUDES(override val sentence: Sentence,
                        v: RDFNode,
                        override val dist: Int,
                        override val score: Double = 1.0)
  /*
   * a known class of a known object
   * e.g. who is the director of the FILM Titanic ?
   */
      extends MainDUDES(sentence, variable = Some(v),
                        score = score, dist = dist)


  case class ClassIncognitaDUDES(override val sentence: Sentence,
                                 v: RDFNode,
                                 override val dist: Int,
                                 override val score: Double = 1.0)
  /*
   * a known class of an unknown object
   * e.g. Which DIRECTOR directed the film Titanc ?
   */
      extends MainDUDES(sentence, variable = Some(v),
                        score = score, dist = dist)

  
  class SolutionGraph(val graph: Graph[MainDUDES, DiEdge]) {
    /**
     * this is more than a data structure: it can be seen as a set of DUDES (and 
     * edges, in other words a graph) with a converter DUDES -> SPARQL
     */

    /*
     * the score is signed in logaritmic scale: it is more substainable in long
     * sentences and the direction is the same: higher is the score, more
     * probable is the match
     */
    val score: Double = graph.nodes.map(n => math.log(n.value.score)).sum
    val length: Int = graph.nodes.length

    def getNode(dudes: MainDUDES): Option[graph.NodeT] =
      /**
       * DUDES -> node
       */
      graph.nodes.toList.filter(node => node.value == dudes).headOption


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
      /**
       * get the variables that are retrieved in a SELECT query; if none the
       * query must be an ASK query
       */
      val incognitaNodes = graph.edges
        .filter(e => e._2.value match {
                  case n: IncognitaDUDES => true
                  case _ => false
                })
        .toList
      /*
       * variables are indicated with an IncognitaDUDES: if it is linked to a
       * relation, it must retrieve the second node of the relation itself,
       * operating as a sort of unknown object; otherwhise get the linked
       * variable
       */
      return incognitaNodes.map(e => e._1.value match {
                                  case n: RelationDUDES => e._2.value.getVariable()
                                  case _ => e._1.value.getVariable()
                                })
        .toSet.toList
    }


    def makeSPARQL(triples: List[String]): String = {
      /**
       * given a list of TRIPLES, write a SPARQL query; the form ASK/SELECT is
       * chosen according to the presence of IncognitaDUDES (see getVariables
       * for more info)
       */
      val queryText = triples.map(triple => s"  $triple").mkString("\n")
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
      /**
       * this function converts the graph into a (hopefully) valid SPARQL query
       */
      def getRelation(n1: MainDUDES, n2: MainDUDES): String = {
        /**
         * get an anonimous relation that connects two objects
         */
        val h1 = getTreeRoot(n1.sentence)
        val h2 = getTreeRoot(n2.sentence)
        return s"?r_${h1}_${h2}"
      }

      var triples = List[String]()

      /* relations
       * the translation begins with relations: they connect two nodes to each
       * other; so it is easy to rewrite the DUDESes as a triple: take the
       * linked nodes and write them with the relation in the middle
       */
      graph.nodes
        .filter(n => n.value match {
                  case n: RelationDUDES => true
                  case _ => false
                })
        .foreach(r => {
                   val edges = r.edges.toList 
                   val edgesIn = edges.filter(e => e._2 == r)
                   val edgesOut = edges.filter(e => e._1 == r)
                   for (eIn  <- edgesIn;
                        eOut <- edgesOut) {
                     val s = eIn._1.value.getVariable()
                     val o = eOut._2.value.getVariable()
                     val triple = s"$s  ${r.value}  $o ."
                     if (! triples.contains(triple))
                       triples = triples :+ triple
                   }
                 })

      /* unknown relations
       * now it is the turn of unknown relations (or in other words all other
       * DUDESes): the name of the relation is generated ad hoc to avoid
       * repetitions (in a very simple way); so two DUDES linked toghether are
       * linked by a new anonimous relation
       * there is the exception (of course) of ClassDUDES, whose relation is
       * known (the "a" relation); and with a simple trick it is possible also
       * to include ClassIncognitaDUDES imponing a more sophisticated relation
       */
      graph.edges
        .filter(e => {
                  val e1: Boolean = e._1.value match {
                    case n: RelationDUDES => false
                    case _ => true
                  }
                  val e2: Boolean = e._2.value match {
                    case n: RelationDUDES => false
                    case n: IncognitaDUDES => false
                    case _ => true
                  }
                  e1 && e2})
        .foreach(e => {
                   val e1 = e._1.value
                   val e2 = e._2.value
                   val r = e2 match {
                     case n: ClassDUDES => "a"
                     case n: ClassIncognitaDUDES => {
                       val newRelation = getRelation(e1, e2)
                       val o = e2.getVariable()
                       s"$newRelation  $o .  $o  a"
                     }
                     case _ => getRelation(e1, e2)
                   }
                   val triple = s"$e1  $r  $e2 ."
                   if (! triples.contains(triple))
                     triples = triples :+ triple
                 })

      /* classes
       * it is possible, if there is a ClassDUDES linked to a IncognitaDUDES,
       * that it is not included in the previous step: it just correct the
       * mistake adding it if not presented yet
       */
      graph.edges
        .filter(n => n._2.value match {
                  case n: ClassDUDES => true
                  case _ => false
                })
        .foreach(n => {
                   val dudes = n._2.value
                   val prev = n._1.value
                   val triple = s"$prev  a  $dudes ."
                   if (! triples.contains(triple))
                     triples = triples :+ triple
                 })

      /*
       * it's all :D
       */
      return makeSPARQL(triples.toList) 
    }


    def printDUDES(): Unit = {
      /**
       * just for debugging pourposes: print a list of DUDES with type and variable
       */
      println("DUDES:")
      val nodes = getNodes().map(n => n.value)
      nodes.foreach(n => {
                      val text: String = n match {
                        case n: ObjectDUDES => "Object"
                        case n: RelationDUDES => "Relation"
                        case n: ClassDUDES => "Class"
                        case n: ClassIncognitaDUDES => "Class of Incognita"
                        case n: IncognitaDUDES => "Incognita"
                      }
                      println(s"$text: ${n.sentence} =[${(100 * n.score).round}%]=> $n")
                    })
      graph.edges.foreach(e => {
                            val from = e._1.value
                            val to = e._2.value
                            println(s"$from ~> $to")
                          })
    }
  }
}
