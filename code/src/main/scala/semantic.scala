package semantics


import org.apache.jena.query.QuerySolution
import org.apache.jena.rdf.model.RDFNode

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._


import nlp._
import main.utils._
import main.constants._


object QASystem {

  def solve(candidates: List[DUDES.SolutionGraph], question: Sentence): Option[DUDES.SolutionGraph] = {
    /**
     * get the best candidate and expand the graph in that direction
     */
    if (printLog()) println(s"\nNew iteration: ${candidates.length} candidates remaining\n")
    if (candidates.isEmpty) {
      if (printLog()) println("No solution found")
      return None
    }
    val maxScore: Double = candidates.map(g => g.score).max
    val bestCandidate: DUDES.SolutionGraph = candidates
      .filter(g => g.score == maxScore)
      .sortBy(g => g.length)
      .last
    val getTopicResults = DUDES.getTopic(bestCandidate, question)
    if (! getTopicResults.isDefined) {  // the best graph is completed, no need to explore further
      if (printLog()) println(s"\n\nSOLUTION FOUND: $question\n\n$bestCandidate")
      return Some(bestCandidate)
    }
    val (topic: DUDES.MainDUDES, sentence: Sentence, up: Boolean) = getTopicResults.get
    val nextSteps: List[Sentence] = exploreTree(sentence + topic.sentence,
                                             topic.sentence,
                                             up)
    if (printLog())
      println(s"checking from $topic:"); nextSteps.foreach(println)

    val bestCandidateLexicalization: Map[String, Map[RDFNode, List[String]]] = Map(
      "relation_in" -> Lexicalization(
        KG(s"""SELECT DISTINCT ?relation ?relation_label WHERE {
              |  $topic ?relation ?object.
              |  ?object a ?class.
              |  OPTIONAL { ?relation rdfs:label ?relation_label. }
              |}""".stripMargin), sentence.lang, "relation"),
      "relation_out" -> Lexicalization(
        KG(s"""SELECT DISTINCT ?relation ?relation_label WHERE {
              |  ?object ?relation $topic.
              |  ?object a ?class.
              |  OPTIONAL { ?relation rdfs:label ?relation_label. }
              |}""".stripMargin), sentence.lang, "relation"),
      "object_in" -> Lexicalization(
        KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
              |  $topic ?relation ?object.
              |  ?object a ?class.
              |  OPTIONAL { ?object rdfs:label ?object_label. }
              |}""".stripMargin), sentence.lang, "object"),
      "object_out" -> Lexicalization(
        KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
              |  ?object ?relation $topic.
              |  ?object a ?class.
              |  OPTIONAL { ?object rdfs:label ?object_label. }
              |}""".stripMargin), sentence.lang, "object"),
      "class_in" -> Lexicalization(
        KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
              |  $topic ?relation ?object.
              |  ?object a ?class.
              |  OPTIONAL { ?class rdfs:label ?class_label. } 
              |}""".stripMargin), sentence.lang, "class"),
      "class_out" -> Lexicalization(
        KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
              |  ?object ?relation $topic.
              |  ?object a ?class.
              |  OPTIONAL { ?class rdfs:label ?class_label. }
              |}""".stripMargin), sentence.lang, "class"))


    val perfectMatches: List[DUDES.SolutionGraph] = nextSteps.map(
      next => PerfectMatch(bestCandidate, topic, bestCandidateLexicalization, next)).flatten

    if (perfectMatches.isEmpty) {
      if (printLog())
        println("Computing fuzzy matches...")

      val fuzzyMatches: List[DUDES.SolutionGraph] = nextSteps.map(
        next => FuzzyMatch(bestCandidate, topic, bestCandidateLexicalization, next)).flatten

      return solve(candidates.filter(c => c != bestCandidate) ++ fuzzyMatches,
                   question)
    }
    return solve(candidates.filter(c => c != bestCandidate) ++ perfectMatches,
                 question)
  }


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
      try {
        val head = tree.dep(position).toInt - 1
        if (head == headQuestion) return acc :+ tree.getPortion((head, head + 1))
        getNext(head, acc :+ tree.getPortion((head, headTopicPosition)))
      } catch {
        case e: java.lang.ArrayIndexOutOfBoundsException => acc 
      }
    }
    return getNext(headTopicPosition, List[Sentence]())
  }

  def exploreTreeDown(tree: Sentence, topic: Sentence): List[Sentence] = {
    /**
     * get a list of candidates for the next step, starting from a TOPIC going down in the TREE
     */
    val ids: Array[String] = tree.id.filterNot(i => topic.id.contains(i))
    val subTrees: Array[Sentence] = splitIntoSubtrees(tree.get(ids))
    def slidingWindow(subtree: Sentence): List[Sentence] = {
      var out: List[Sentence] = List(subtree)
      for (i <- 0 until subtree.length;
         size <- 1 to (subtree.length - i)) {
        val nextSentence = subtree.getPortion(i, i + size)
        if (nextSentence.isValidTree) out = out :+ nextSentence
      }
      return out
    }
    return subTrees.map(slidingWindow).toList.flatten
  }
    // add a sliding window to check all the sub-trees

  def exploreTree(tree: Sentence, topic: Sentence, up: Boolean): List[Sentence] =
    if (up) exploreTreeUp(tree, topic)
    else exploreTreeDown(tree, topic)

  def apply(question: String): String = {
    val tree: Sentence = Parser(question)
    val sentenceGraphs: List[DUDES.SolutionGraph] = NEE(tree)
    val solution: Option[DUDES.SolutionGraph] = solve(sentenceGraphs, tree)
    if (solution.isDefined) {
      val solutionGraph = solution.get
      val variables = solutionGraph.getVariables()
      val solutions: List[QuerySolution] = KG(solution.get.toString)
      return solutions.map(q => {
                             val solutions = variables.map(v => q.get(v))
                             solutions.mkString(", ")
                           }).mkString("\n")
    }
    return "<!> ERROR: no answer for this question."
  }

}


object PerfectMatch {

  def perfectMatch(candidate: String, possibilities: List[String]): Boolean =
    possibilities.map(p => p.toLowerCase()).contains(candidate.toLowerCase())

  def getRelations(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES,
                 lexicon: Map[String, Map[RDFNode, List[String]]], sentence: Sentence):
      List[DUDES.SolutionGraph] = {
    val relationsIn = lexicon("relation_in")
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
    val relationsOut = lexicon("relation_out")
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

  def getObjects(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES,
               lexicon: Map[String, Map[RDFNode, List[String]]], sentence: Sentence):
      List[DUDES.SolutionGraph] = {
    val objectsIn = lexicon("object_in")
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
    val objectsOut = lexicon("object_out")
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

  def getClasses(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES,
               lexicon: Map[String, Map[RDFNode, List[String]]], sentence: Sentence):
      List[DUDES.SolutionGraph] = {
    val classesIn = lexicon("class_in")
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
    val classesOut = lexicon("class_out")
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
  

  def apply(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES,
          lexicon: Map[String, Map[RDFNode, List[String]]], nextStepTree: Sentence):
      List[DUDES.SolutionGraph] = {
    if ((nextStepTree.length == 1 &&  // if the nextStep is a interrogative pronoun
          nextStepTree.pos.head == "PRON" &&
           nextStepTree.flex.head == "PronType=Int") ||
          false) {
      val newDudes = new DUDES.IncognitaDUDES(nextStepTree, topic.dist + 1)
      val newSolution = candidate.addDUDES(topic ~> newDudes)
      if (newSolution.isDefined) return List(newSolution.get)
    }
    val out = getRelations(candidate, topic, lexicon, nextStepTree) ++
      getObjects(candidate, topic, lexicon, nextStepTree) ++
      getClasses(candidate, topic, lexicon, nextStepTree)
    if (printLog()) out.foreach(println)
    return out
  }

}

object FuzzyMatch {
  def getRelations(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES,
                 lexicon: Map[String, Map[RDFNode, List[String]]], sentence: Sentence):
      List[DUDES.SolutionGraph] = ???
  def getObjects(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES,
                 lexicon: Map[String, Map[RDFNode, List[String]]], sentence: Sentence):
      List[DUDES.SolutionGraph] = ???
  def getClasses(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES,
                 lexicon: Map[String, Map[RDFNode, List[String]]], sentence: Sentence):
      List[DUDES.SolutionGraph] = ???

  def apply(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES,
          lexicon: Map[String, Map[RDFNode, List[String]]], nextStepTree: Sentence):
      List[DUDES.SolutionGraph] = {
    return List(candidate)
    val out = (getRelations(candidate, topic, lexicon, nextStepTree) ++
                 getObjects(candidate, topic, lexicon, nextStepTree) ++
                 getClasses(candidate, topic, lexicon, nextStepTree))
    return out.filter(g => g.score > 0)
  }
}
