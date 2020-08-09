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

  def getTopic(candidate: DUDES.SolutionGraph, question: Sentence):
      Option[(DUDES.MainDUDES, Sentence, Boolean)] = {
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
                                       if (printLog()) println(s"$node: $s")
                                       return Some(node, s, false)
                                     }
                                   })
        // node -[depends by]-> sentence
        val head = getTreeRoot(node.sentence)
        val dependsBy = node.sentence.get(head).dep.head
        remainingSentences.foreach(s => {
                                     if (s.id.contains(dependsBy)) {
                                       if (printLog()) println(s"$node: $s")
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
      s"${prev.getObjectDUDES()}  ${prev.getRelationDUDES()}  ${getObjectDUDES()}"
    def toRDF(): String = getObjectDUDES()

    def apply(): RDFNode = o.get
  }

  case class IncognitaDUDES(override val sentence: Sentence,
                            override val dist: Int)
      extends MainDUDES(sentence,
                        dist = dist) {
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
      s"""${prev.getObjectDUDES()}  ${prev.getRelationDUDES()}  ${getObjectDUDES()} .
         |${getObjectDUDES()}  a  ${getClassDUDES()}""".stripMargin

    override def toString(): String = s"<${c.get}>"
  }

  class SolutionGraph(val graph: Graph[MainDUDES, DiEdge]) {

    // the score is signed in logaritmic scale: it is more substainable in long sentences
    // and the direction is the same: higher is the score, more probable is the match
    val score: Double = graph.nodes.map(n => math.log(n.value.score)).sum

    def getNode(dudes: DUDES.MainDUDES): Option[graph.NodeT] = {
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

    def makeRDF(query: List[String]): String = {
      val incognitaNodes: List[MainDUDES] = graph.nodes
        .map(n => n.value)
        .filter(n => n match {
                  case n: IncognitaDUDES => true
                  case _ => false
                }).toList

      val queryText = query.map(triple => s"  $triple").mkString("\n")

      if (incognitaNodes.isEmpty)
        return s"""ASK WHERE {
                  |$queryText
                  |}""".stripMargin
      val incognita: String = incognitaNodes.map(dudes => dudes.toRDF()).mkString(" ")
      return s"""SELECT DISTINCT $incognitaNodes WHERE {
                |$queryText
                |}"""
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


  def exploreTreeUp(tree: Sentence, topic: Sentence): List[Sentence] = {
    /**
     * get a list of candidates for the next step, starting from a TOPIC going up in the TREE
     */
    val headQuestion = getTreeRoot(tree).toInt - 1
    val headTopic = getTreeRoot(topic)
    val headTopicPosition = headTopic.toInt - 1 

    def getNext(position: Int, acc: List[Sentence]): List[Sentence] = {
      val head = tree.dep(position).toInt - 1
      if (head == headQuestion) return acc :+ tree.getPortion((head, head + 1))
      getNext(head, acc :+ tree.getPortion((head, headTopicPosition)))
    }
    return getNext(headTopicPosition, List[Sentence]())
  }

  def exploreTreeDown(tree: Sentence, topic: Sentence): List[Sentence] = ???
  // add a sliding window to check all the sub-trees

  def exploreTree(tree: Sentence, topic: Sentence, up: Boolean): List[Sentence] =
    if (up) exploreTreeUp(tree, topic)
    else exploreTreeDown(tree, topic)


  def solve(explored: List[DUDES.SolutionGraph]): DUDES.SolutionGraph = {
    val maxScore: Double = explored.map(sg => sg.score).max
    val toProcess: List[DUDES.SolutionGraph] = explored.filter(sg => sg.score == maxScore) 
    return toProcess.head
  }


  def apply(question: String): String = {
    val tree: Sentence = Parser(question)
    val sentenceGraphs: List[DUDES.SolutionGraph] = NEE(tree)
    sentenceGraphs.foreach(g => PerfectMatch(g, tree))
    return "42"
  }

}


object PerfectMatch {

  def perfectMatch(candidate: String, possibilities: List[String]): Boolean =
    possibilities.map(p => p.toLowerCase()).contains(candidate.toLowerCase())

  def getRelations(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES, sentence: Sentence, up: Boolean):
      List[DUDES.SolutionGraph] = {
    val relationsIn = Lexicalization(
      KG(s"""SELECT DISTINCT ?relation ?relation_label WHERE {
            |  $topic ?relation ?object.
            |  ?object a ?class.
            |  OPTIONAL { ?relation rdfs:label ?relation_label. }
            |}""".stripMargin).toList, sentence.lang, "relation")
      .filter(kv => perfectMatch(sentence.sentence, kv._2))
      .map(kv => kv._1)
      .map(r => {
             val newDudes = new DUDES.RelationDUDES(sentence, r,
                                                    topic.dist + 1, 1.0)
             val edge = topic ~> newDudes
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    val relationsOut = Lexicalization(
      KG(s"""SELECT DISTINCT ?relation ?relation_label WHERE {
            |  ?object ?relation $topic.
            |  ?object a ?class.
            |  OPTIONAL { ?relation rdfs:label ?relation_label. }
            |}""".stripMargin).toList, sentence.lang, "relation")
      .filter(kv => perfectMatch(sentence.sentence, kv._2))
      .map(kv => kv._1)
      .map(r => {
             val newDudes = new DUDES.RelationDUDES(sentence, r,
                                                    topic.dist + 1, 1.0)
             val edge = newDudes ~> topic
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    return (relationsIn ++ relationsOut).toList
  }

  def getObjects(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES, sentence: Sentence, up: Boolean):
      List[DUDES.SolutionGraph] = {
    val objectsIn = Lexicalization(
      KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
            |  $topic ?relation ?object.
            |  ?object a ?class.
            |  OPTIONAL { ?object rdfs:label ?object_label. }
            |}""".stripMargin).toList, sentence.lang, "object")
      .filter(kv => perfectMatch(sentence.sentence, kv._2))
      .map(kv => kv._1)
      .map(o => {
             val newDudes = new DUDES.ObjectDUDES(sentence, o,
                                                  topic.dist + 1, 1.0)
             val edge = topic ~> newDudes
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    val objectsOut = Lexicalization(
      KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
            |  ?object ?relation $topic.
            |  ?object a ?class.
            |  OPTIONAL { ?object rdfs:label ?object_label. }
            |}""".stripMargin).toList, sentence.lang, "object")
      .filter(kv => perfectMatch(sentence.sentence, kv._2))
      .map(kv => kv._1)
      .map(o => {
             val newDudes = new DUDES.ObjectDUDES(sentence, o,
                                                  topic.dist + 1, 1.0)
             val edge = newDudes ~> topic
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    return (objectsIn ++ objectsOut).toList
  }

  def getClasses(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES, sentence: Sentence, up: Boolean):
      List[DUDES.SolutionGraph] = {
    val classesIn = Lexicalization(
      KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
            |  $topic ?relation ?object.
            |  ?object a ?class.
            |  OPTIONAL { ?class rdfs:label ?class_label. } 
            |}""".stripMargin).toList, sentence.lang, "class")
      .filter(kv => perfectMatch(sentence.sentence, kv._2))
      .map(kv => kv._1)
      .map(c => {
             val newDudes = new DUDES.ClassDUDES(sentence, c,
                                                 topic.dist + 1, 1.0)
             val edge = topic ~> newDudes
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    val classesOut = Lexicalization(
      KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
            |  ?object ?relation $topic.
            |  ?object a ?class.
            |  OPTIONAL { ?class rdfs:label ?class_label. }
            |}""".stripMargin).toList, sentence.lang, "class")
      .filter(kv => perfectMatch(sentence.sentence, kv._2))
      .map(kv => kv._1)
      .map(c => {
             val newDudes = new DUDES.ClassDUDES(sentence, c,
                                                 topic.dist + 1, 1.0)
             val edge = newDudes ~> topic
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    return (classesIn ++ classesOut).toList
  }
  

  def apply(candidate: DUDES.SolutionGraph, question: Sentence): List[DUDES.SolutionGraph] = {
    val getTopicResults = DUDES.getTopic(candidate, question)
    if (! getTopicResults.isDefined) return List[DUDES.SolutionGraph]()
    val (topic: DUDES.MainDUDES, sentence: Sentence, up: Boolean) = getTopicResults.get
    val nextStepTree: Sentence = QASystem.exploreTree(sentence + topic.sentence,
                                                      topic.sentence,
                                                      up).head
    val out = getRelations(candidate, topic, nextStepTree, up) ++
      getObjects(candidate, topic, nextStepTree, up) ++
      getClasses(candidate, topic, nextStepTree, up)
    if (printLog()) out.foreach(println)
    return out
  }

}
