package semantics


import org.apache.jena.query.QuerySolution
import org.apache.jena.rdf.model.RDFNode

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._


import nlp._
import main.utils._
import main.constants._


object QASystem {

  def expandGraph(node: RDFNode, out: Boolean): List[QuerySolution] = {
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
    return KG(query)
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
            |}""".stripMargin), sentence.lang, "relation")
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
            |}""".stripMargin), sentence.lang, "relation")
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
            |}""".stripMargin), sentence.lang, "object")
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
            |}""".stripMargin), sentence.lang, "object")
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
            |}""".stripMargin), sentence.lang, "class")
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
            |}""".stripMargin), sentence.lang, "class")
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
