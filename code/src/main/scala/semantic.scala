package semantics


import org.apache.jena.query.QuerySolution
import org.apache.jena.rdf.model.RDFNode

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._


import nlp._
import main.utils._
import main.constants._


object QASystem {

  def getSolution(solution: DUDES.SolutionGraph): Option[String] = {
    val variables = solution.getVariables()
    val query = solution.toString()
    if (query.startsWith("SELECT")) {
      val out = KG.querySelect(query)
      if (out.isEmpty) return None
      return Some(out.map(q => {
                            variables.map(v => q.get(v).toString).mkString(", ")
                          }).mkString("\n"))
    }
    return Some(KG.queryAsk(query).toString)
  }

  def solve(candidates: List[DUDES.SolutionGraph], question: Sentence): Option[(DUDES.SolutionGraph, String)] = {
    /**
     * get the best candidate and expand the graph in that direction
     */
    if (printLog()) println(s"\nNew iteration: ${candidates.length} candidates remaining\n")
    if (candidates.isEmpty) {
      if (printLog()) println("No solution found")
      return null
    }
    val maxScore: Double = candidates.map(g => g.score).maxBy(math.exp)
    val bestCandidate: DUDES.SolutionGraph = candidates
      .filter(g => g.score == maxScore)
      .sortBy(g => g.length)
      .last
    val getTopicResults = DUDES.getTopic(bestCandidate, question)
    if (! getTopicResults.isDefined) {  // the best graph is completed, no need to explore further
      if (printLog()) {
        bestCandidate.printDUDES()
        println(s"\n\nSOLUTION FOUND [${math.exp(bestCandidate.score)}]: $question\n\n$bestCandidate")
      }
      val out = getSolution(bestCandidate)
      if (! out.isDefined) return solve(candidates.filter(c => c != bestCandidate), question)
      return Some((bestCandidate, out.get))
    }
    val (topic: DUDES.MainDUDES, sentence: Sentence, up: Boolean) = getTopicResults.get
    val nextSteps: List[Sentence] = exploreTree(sentence + topic.sentence,
                                             topic.sentence,
                                             up)
    if (printLog()) {
      println(bestCandidate)
      bestCandidate.printDUDES()
      println(s"checking from $topic:"); nextSteps.foreach(println)
    }

    if (nextSteps.filter(s => s.id.exists(POS.openClassPOS)).isEmpty) {
      nextSteps.foreach(nextStepTree =>
        if (POS.isInterrogative(nextStepTree)) {
          val newDudes = new DUDES.IncognitaDUDES(nextStepTree, topic.dist + 1)
          val newSolution = bestCandidate.addDUDES(topic ~> newDudes)
          if (newSolution.isDefined)
            return solve(candidates.filter(_ != bestCandidate) :+ newSolution.get, question)
        })
    }

    val bestCandidateLexicalization: Map[String, Map[RDFNode, List[String]]] = topic match {
      case n: DUDES.RelationDUDES => {
        val edge1 = bestCandidate.graph.edges.filter(e => e._1.value == topic).headOption
        val edge2 = bestCandidate.graph.edges.filter(e => e._2.value == topic).headOption
        if (edge1.isDefined) Map(
          "relation_in" -> Map[RDFNode, List[String]](),
          "relation_out" -> Map[RDFNode, List[String]](),
          "object_in" -> Map[RDFNode, List[String]](),
          "object_out" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
                  |  ?object $topic ${edge1.get._2.value}.
                  |  ?object a ?class.
                  |  OPTIONAL { ?object rdfs:label ?object_label. }
                  |}""".stripMargin), sentence.lang, "object"),
          "class_in" -> Map[RDFNode, List[String]](),
          "class_out" -> Map[RDFNode, List[String]](),
          "class_incognita_in" -> Map[RDFNode, List[String]](),
          "class_incognita_out" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                  |  ?object $topic ${edge1.get._2.value}.
                  |  ?object a ?class.
                  |  OPTIONAL { ?class rdfs:label ?class_label. }
                  |}""".stripMargin), sentence.lang, "class"))
        else if (edge2.isDefined ) Map(
          "relation_in" -> Map[RDFNode, List[String]](),
          "relation_out" -> Map[RDFNode, List[String]](),
          "object_in" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
                  |  ${edge2.get._1.value} $topic ?object.
                  |  ?object a ?class.
                  |  OPTIONAL { ?object rdfs:label ?object_label. }
                  |}""".stripMargin), sentence.lang, "object"),
          "object_out" -> Map[RDFNode, List[String]](),
          "class" -> Map[RDFNode, List[String]](),
          "class_incognita_in" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                  |  ${edge2.get._1.value} $topic ?object.
                  |  ?object a ?class.
                  |  OPTIONAL { ?class rdfs:label ?class_label. }
                  |}""".stripMargin), sentence.lang, "class"),
          "class_incognita_out" -> Map[RDFNode, List[String]]())
        else Map(
          "relation_in" -> Map[RDFNode, List[String]](),
          "relation_out" -> Map[RDFNode, List[String]](),
          "object_in" -> Map[RDFNode, List[String]](),
          "object_out" -> Map[RDFNode, List[String]](),
          "class" -> Map[RDFNode, List[String]](),
          "class_incognita_in" -> Map[RDFNode, List[String]](),
          "class_incognita_out" -> Map[RDFNode, List[String]]())
      }
      case _ => {
        Map(
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
                  |  FILTER NOT EXISTS { $topic  a  ?object. }
                  |}""".stripMargin), sentence.lang, "object"),
          "object_out" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
                  |  ?object ?relation $topic.
                  |  ?object a ?class.
                  |  OPTIONAL { ?object rdfs:label ?object_label. }
                  |  FILTER NOT EXISTS { $topic  a  ?object. }
                  |}""".stripMargin), sentence.lang, "object"),
          "class" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                  |  $topic a ?class.
                  |  OPTIONAL { ?class rdfs:label ?class_label. } 
                  |}""".stripMargin), sentence.lang, "class"),
          "class_incognita_in" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                  |  $topic ?relation ?object.
                  |  ?object a ?class.
                  |  OPTIONAL { ?class rdfs:label ?class_label. } 
                  |}""".stripMargin), sentence.lang, "class"),
          "class_incognita_out" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                  |  ?object ?relation $topic.
                  |  ?object a ?class.
                  |  OPTIONAL { ?class rdfs:label ?class_label. }
                  |}""".stripMargin), sentence.lang, "class"))
      }      
    }

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


  def exploreTree(tree: Sentence, topic: Sentence, up: Boolean): List[Sentence] = {
    /**
     * get a list of candidates for the next step, starting from a TOPIC going down in the TREE
     */
    val ids: Array[String] = tree.id.filterNot(i => topic.id.contains(i))
    val subTrees: Array[Sentence] = splitIntoSubtrees(tree.get(ids))

    def check(sentence: Sentence): Boolean =
      sentence.isValidTree && sentence.pos.exists(POS.openClassPOS)

    def slidingWindow(subtree: Sentence): List[Sentence] = {
      var out: List[Sentence] = List(subtree)
      for (i <- 0 until subtree.length;
         size <- 1 to (subtree.length - i)) {
        val nextSentence = subtree.getPortion(i, i + size)
        if (nextSentence.id.head.toInt < topic.id.head.toInt) {
          val p0 = tree.id.indexOf(nextSentence.id.head)
          val p1 = tree.id.indexOf(topic.id.head)
          val infra = tree.getPortion(p0, p1)
          if (! infra.id.exists(POS.openClassPOS) && check(nextSentence))
            out = out :+ nextSentence
        } else {
          val p0 = tree.id.indexOf(topic.id.head)
          val p1 = tree.id.indexOf(nextSentence.id.head)
          val infra = tree.getPortion(p0, p1)
          if (! infra.id.exists(POS.openClassPOS) && check(nextSentence))
            out = out :+ nextSentence
        }
      }
      return out
    }

    return subTrees.map(slidingWindow).toList.flatten
  }

  def apply(question: String): String = {
    val tree: Sentence = Parser(question)
    val sentenceGraphs: List[DUDES.SolutionGraph] = NEE(tree)
    val solutionCandidate: Option[(DUDES.SolutionGraph, String)] = solve(sentenceGraphs, tree)
    if (solutionCandidate.isDefined) {
      val solution = solutionCandidate.get
      return solution._2
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
    val classes = lexicon("class")
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
    return classes.toList
  }

  def getIncognitaClasses(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES,
                        lexicon: Map[String, Map[RDFNode, List[String]]], sentence: Sentence):
      List[DUDES.SolutionGraph] = {
    val classesIn = lexicon("class_incognita_in")
      .filter(kv => perfectMatch(sentence.sentence, kv._2))
      .map(kv => kv._1)
      .map(c => {
             val newDudes = new DUDES.ClassIncognitaDUDES(sentence, c,
                                                          topic.dist + 1, 1.0)
             val edge = topic ~> newDudes
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    val classesOut = lexicon("class_incognita_out")
      .filter(kv => perfectMatch(sentence.sentence, kv._2))
      .map(kv => kv._1)
      .map(c => {
             val newDudes = new DUDES.ClassIncognitaDUDES(sentence, c,
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
    val out = getRelations(candidate, topic, lexicon, nextStepTree) ++
      getObjects(candidate, topic, lexicon, nextStepTree) ++
      getClasses(candidate, topic, lexicon, nextStepTree) ++
      getIncognitaClasses(candidate, topic, lexicon, nextStepTree)
    return out
  }

}

object FuzzyMatch {
  def getRelations(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES, topicLabel: String,
                 lexicon: Map[String, Map[RDFNode, List[String]]], sentence: Sentence):
      List[DUDES.SolutionGraph] =  {
    val scores: Map[RDFNode, Double] = Encoder(lexicon("relation_in") ++ lexicon("relation_out"),
                                               sentence,
                                               topicLabel)
    val relationsIn = lexicon("relation_in")
      .map(kv => {
             val newDudes = new DUDES.RelationDUDES(sentence, kv._1,
                                                    topic.dist + 1, scores(kv._1))
             val edge = topic ~> newDudes
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    val relationsOut = lexicon("relation_out")
      .map(kv => {
             val newDudes = new DUDES.RelationDUDES(sentence, kv._1,
                                                    topic.dist + 1, scores(kv._1))
             val edge = newDudes ~> topic
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    return (relationsIn ++ relationsOut).toList
  }

  def getObjects(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES, topicLabel: String,
                 lexicon: Map[String, Map[RDFNode, List[String]]], sentence: Sentence):
      List[DUDES.SolutionGraph] = {
    val scores: Map[RDFNode, Double] = Encoder(lexicon("object_in") ++ lexicon("object_out"),
                                               sentence,
                                               topicLabel)
    val objectsIn = lexicon("object_in")
      .map(kv => {
             val newDudes = new DUDES.ObjectDUDES(sentence, kv._1,
                                                  topic.dist + 1, scores(kv._1))
             val edge = topic ~> newDudes
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    val objectsOut = lexicon("object_out")
      .map(kv => {
             val newDudes = new DUDES.ObjectDUDES(sentence, kv._1,
                                                  topic.dist + 1, scores(kv._1))
             val edge = newDudes ~> topic
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    return (objectsIn ++ objectsOut).toList
  }

  def getClasses(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES, topicLabel: String,
                 lexicon: Map[String, Map[RDFNode, List[String]]], sentence: Sentence):
      List[DUDES.SolutionGraph] = {
    val scores: Map[RDFNode, Double] = Encoder(lexicon("class"),
                                               sentence,
                                               topicLabel)
    val classes = lexicon("class")
      .map(kv => {
             val newDudes = new DUDES.ClassDUDES(sentence, kv._1,
                                                 topic.dist + 1, scores(kv._1))
             val edge = topic ~> newDudes
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    return classes.toList
  }

  def getIncognitaClasses(candidate: DUDES.SolutionGraph, topic: DUDES.MainDUDES, topicLabel: String,
                        lexicon: Map[String, Map[RDFNode, List[String]]], sentence: Sentence):
      List[DUDES.SolutionGraph] = {
    val scores: Map[RDFNode, Double] = Encoder(lexicon("class_incognita_in") ++ lexicon("class_incognita_out"),
                                               sentence,
                                               topicLabel)
    val classesIn = lexicon("class_in")
      .map(kv => {
             val newDudes = new DUDES.ClassIncognitaDUDES(sentence, kv._1,
                                                          topic.dist + 1, scores(kv._1))
             val edge = topic ~> newDudes
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    val classesOut = lexicon("class_out")
      .map(kv => {
             val newDudes = new DUDES.ClassIncognitaDUDES(sentence, kv._1,
                                                          topic.dist + 1, scores(kv._1))
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
    val topicString = Lexicalization(topic, topic.sentence.lang)
    val out = (getRelations(candidate, topic, topicString, lexicon, nextStepTree) ++
                 getObjects(candidate, topic, topicString, lexicon, nextStepTree) ++
                 getClasses(candidate, topic, topicString, lexicon, nextStepTree) ++
                 getIncognitaClasses(candidate, topic, topicString, lexicon, nextStepTree))
    return out.filter(g => math.exp(g.score) > 0)
  }
}
