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

  def solve(candidates: List[DUDES.SolutionGraph], question: Sentence, logger: (String)=>String):
      Option[(DUDES.SolutionGraph, String)] = {
    /**
     * get the best candidate and expand the graph in that direction
     */

    if (candidates.isEmpty) {
      /*
       * it is not possible to check in any dierction now
       */
      logger("No solution found")
      return None
    }

    /*
     * it takes the most promising candidate (Dijkstra algorithm): the one with
     * the higher score and the higher lenght
     */
    val maxScore: Double = candidates.map(g => g.score).maxBy(math.exp)
    logger(s"\nNew iteration: ${candidates.length} candidates remaining =[score]=> $maxScore\n")
    val bestCandidate: DUDES.SolutionGraph = candidates
      .filter(g => g.score == maxScore)
      .sortBy(g => g.length)
      .last

    // it now takes the expansion direction
    val getTopicResults = DUDES.getTopic(bestCandidate, question)


    if (! getTopicResults.isDefined) {
      // the best graph is completed, no need to explore further
      bestCandidate.printDUDES(logger)
      logger(s"\n\nSOLUTION FOUND [${math.exp(bestCandidate.score)}]: $question\n\n$bestCandidate")
      /*
       * if the query is well formulated, it retrieves an answer: there is the
       * possibility that the graph is not well formulated; in that case the
       * algorithm does not stops but it ignores the candidate
       */
      val out = getSolution(bestCandidate)

      if (! out.isDefined)
        return solve(candidates.filter(c => c != bestCandidate), question, logger)
      return Some((bestCandidate, out.get))
    }

    // if the graph can be expanded, take the direction
    val (topic: DUDES.MainDUDES, sentence: Sentence) = getTopicResults.get
    val nextSteps: List[Sentence] = exploreTree(sentence + topic.sentence,
                                                topic.sentence,
                                                question)

    logger(s"$bestCandidate")
    bestCandidate.printDUDES(logger)

    /*
     * checks if there is an interrogative pronoun/adverb/something that
     * indicates what to retrieve as answer: in that case does not expand the
     * graph but insert a sign with the variable to retrieve
     */
    if (! nextSteps.exists(word => word.pos.exists(POS.openClassPOS))) {
      nextSteps.foreach(nextStepTree =>
        if (POS.isInterrogative(nextStepTree)) {
          val newDudes = new DUDES.IncognitaDUDES(nextStepTree, topic.dist + 1)
          val newSolution = bestCandidate.addDUDES(topic ~> newDudes)
          if (newSolution.isDefined)
            return solve(candidates.filter(_ != bestCandidate) :+ newSolution.get,
                         question,
                         logger)
        })
      val out = getSolution(bestCandidate)
      if (! out.isDefined)
        return solve(candidates.filter(c => c != bestCandidate), question, logger)
      return Some((bestCandidate, out.get))
    }

    logger(s"checking from $topic: \n" + nextSteps.map(s => s" - $s").mkString("\n"))
    /*
     * a boring part: it just takes a list of nodes and lexicalizations for all
     * possible expansions in the graph, depending on the class of the topic
     * DUDES and the possible candidates in the following step
     */
    logger("retrieving candidates...")
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
                  |  OPTIONAL { ?object $labelRelation ?object_label. }
                  |}""".stripMargin), sentence.lang, "object"),
          "class" -> Map[RDFNode, List[String]](),
          "class_incognita_in" -> Map[RDFNode, List[String]](),
          "class_incognita_out" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                  |  ?object $topic ${edge1.get._2.value}.
                  |  ?object a ?class.
                  |  OPTIONAL { ?class $labelRelation ?class_label. }
                  |}""".stripMargin), sentence.lang, "class"))
        else if (edge2.isDefined) Map(
          "relation_in" -> Map[RDFNode, List[String]](),
          "relation_out" -> Map[RDFNode, List[String]](),
          "object_in" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
                  |  ${edge2.get._1.value} $topic ?object.
                  |  ?object a ?class.
                  |  OPTIONAL { ?object $labelRelation ?object_label. }
                  |}""".stripMargin), sentence.lang, "object"),
          "object_out" -> Map[RDFNode, List[String]](),
          "class" -> Map[RDFNode, List[String]](),
          "class_incognita_in" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                  |  ${edge2.get._1.value} $topic ?object.
                  |  ?object a ?class.
                  |  OPTIONAL { ?class $labelRelation ?class_label. }
                  |}""".stripMargin), sentence.lang, "class"),
          "class_incognita_out" -> Map[RDFNode, List[String]]())
        else Map(
          "relation_in" -> Map[RDFNode, List[String]](),
          "relation_out" -> Map[RDFNode, List[String]](),
          "object_in" ->  Lexicalization(
            KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
                  |  ?subject $topic ?object.
                  |  ?object a ?class.
                  |  OPTIONAL { ?object $labelRelation ?object_label. }
                  |}""".stripMargin), sentence.lang, "object"),
          "object_out" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
                  |  ?object $topic ?subject.
                  |  ?object a ?class.
                  |  OPTIONAL { ?object $labelRelation ?object_label. }
                  |}""".stripMargin), sentence.lang, "object"),
          "class" -> Map[RDFNode, List[String]](),
          "class_incognita_in" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                  |  ?subject $topic ?object.
                  |  ?object a ?class.
                  |  OPTIONAL { ?class $labelRelation ?class_label. }
                  |}""".stripMargin), sentence.lang, "class"),
          "class_incognita_out" -> Lexicalization(
            KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                  |  ?object $topic ?subject.
                  |  ?object a ?class.
                  |  OPTIONAL { ?class $labelRelation ?class_label. }
                  |}""".stripMargin), sentence.lang, "class"))
      }

      case n: DUDES.ClassIncognitaDUDES =>
        Map(
        "relation_in" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?relation ?relation_label WHERE {
                |  ?subject a $topic.
                |  ?subject ?relation ?object.
                |  ?object a ?class.
                |  OPTIONAL { ?relation $labelRelation ?relation_label. }
                |}""".stripMargin), sentence.lang, "relation"),
        "relation_out" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?relation ?relation_label WHERE {
                |  ?subject a $topic.
                |  ?object ?relation ?subject.
                |  ?object a ?class.
                |  OPTIONAL { ?relation $labelRelation ?relation_label. }
                |}""".stripMargin), sentence.lang, "relation"),
        "object_in" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
                |  ?subject a $topic.
                |  ?subject ?relation ?object.
                |  OPTIONAL { ?object $labelRelation ?object_label. }
                |  FILTER NOT EXISTS { ?subject  a  ?object. }
                |}""".stripMargin), sentence.lang, "object"),
        "object_out" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
                |  ?subject a $topic.
                |  ?object ?relation ?subject.
                |  OPTIONAL { ?object $labelRelation ?object_label. }
                |  FILTER NOT EXISTS { ?subject  a  ?object. }
                |}""".stripMargin), sentence.lang, "object"),
        "class" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                |  ?subject a $topic.
                |  ?subject a ?class.
                |  OPTIONAL { ?class $labelRelation ?class_label. } 
                |}""".stripMargin), sentence.lang, "class"),
        "class_incognita_in" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                |  ?subject a $topic.
                |  ?subject ?relation ?object.
                |  ?object a ?class.
                |  OPTIONAL { ?class $labelRelation ?class_label. } 
                |}""".stripMargin), sentence.lang, "class"),
        "class_incognita_out" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                |  ?subject a $topic.
                |  ?object ?relation ?subject.
                |  ?object a ?class.
                |  OPTIONAL { ?class $labelRelation ?class_label. }
                |}""".stripMargin), sentence.lang, "class"))

      case _ => Map(
        "relation_in" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?relation ?relation_label WHERE {
                |  $topic ?relation ?object.
                |  OPTIONAL { ?relation $labelRelation ?relation_label. }
                |}""".stripMargin), sentence.lang, "relation"),
        "relation_out" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?relation ?relation_label WHERE {
                |  ?object ?relation $topic.
                |  OPTIONAL { ?relation $labelRelation ?relation_label. }
                |}""".stripMargin), sentence.lang, "relation"),
        "object_in" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
                |  $topic ?relation ?object.
                |  OPTIONAL { ?object $labelRelation ?object_label. }
                |  FILTER NOT EXISTS { $topic  a  ?object. }
                |}""".stripMargin), sentence.lang, "object"),
        "object_out" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?object ?object_label WHERE {
                |  ?object ?relation $topic.
                |  OPTIONAL { ?object $labelRelation ?object_label. }
                |  FILTER NOT EXISTS { $topic  a  ?object. }
                |}""".stripMargin), sentence.lang, "object"),
        "class" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                |  $topic a ?class.
                |  OPTIONAL { ?class $labelRelation ?class_label. } 
                |}""".stripMargin), sentence.lang, "class"),
        "class_incognita_in" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                |  $topic ?relation ?object.
                |  ?object a ?class.
                |  OPTIONAL { ?class $labelRelation ?class_label. } 
                |}""".stripMargin), sentence.lang, "class"),
        "class_incognita_out" -> Lexicalization(
          KG(s"""SELECT DISTINCT ?class ?class_label WHERE {
                |  ?object ?relation $topic.
                |  ?object a ?class.
                |  OPTIONAL { ?class $labelRelation ?class_label. }
                |}""".stripMargin), sentence.lang, "class"))
    }

    /*
     * check first for entities written exactly as is: it is a very simple
     * answer to the OOV problem (OOV terms are, generally speaking, thecnical
     * terms, so a perfect correspondence is needed)
     */
    logger("Computing perfect matches...")
    val perfectMatches: List[DUDES.SolutionGraph] = nextSteps.map(
      next => PerfectMatch(bestCandidate, topic, bestCandidateLexicalization, next)).flatten

    /*
     * it now uses a NN in order to obtain a list of links where the
     * correspondence is not perfect: it just uses a cosine similarity between
     * text and lexicalization of the KG as heuristics
     */
    if (perfectMatches.isEmpty) {
      // uncomment to make fast tests
      // return solve(candidates.filter(c => c != bestCandidate), question, logger)

      logger("Computing fuzzy matches...")

      val fuzzyMatches: List[DUDES.SolutionGraph] = nextSteps.map(
        next => FuzzyMatch(bestCandidate, topic, bestCandidateLexicalization, next)).flatten

      return solve(candidates.filter(c => c != bestCandidate) ++ fuzzyMatches,
                   question,
                   logger)
    }
    return solve(candidates.filter(c => c != bestCandidate) ++ perfectMatches,
                 question,
                 logger)
  }


  def expandGraph(node: DUDES.MainDUDES, out: Boolean): List[QuerySolution] = {
    /**
     * expand the graph starting from a NODE in either directions in or OUT
     */
    val queryDirection = out match {
      case true => node match {
        case n: DUDES.RelationDUDES =>       s"?subject $node ?object."

        case n: DUDES.ClassIncognitaDUDES => s"?subject ?relation ?object. \n  " +
                                             s"?subject a $node."

        case _ =>                            s"$node ?relation ?object."
      }
      case false => node match {
        case n: DUDES.RelationDUDES =>       s"?object $node ?subject."

        case n: DUDES.ClassIncognitaDUDES => s"?object ?relation ?subject. \n  " +
                                             s"?subject a $node."

        case _ =>                            s"?object ?realtion $node."
      }
    }
    val query = s"""SELECT ?relation ?object ?class
                   |WHERE {
                   |  $queryDirection
                   |  OPTIONAL { ?object a ?class. }
                   |}""".stripMargin
    return KG(query)
  }


  def exploreTree(tree: Sentence, topic: Sentence, question: Sentence): List[Sentence] = {
    /**
     * get a list of candidates for the next step, starting from a TOPIC going down in the TREE
     */
    val ids: Array[String] = tree.id.filterNot(i => topic.id.contains(i))
    val subTrees: Array[Sentence] = splitIntoSubtrees(tree.get(ids))

    def check(sentence: Sentence): Boolean =
      /**
       * check if a SENTENCE can be considered a candidate
       */
      sentence.isValidTree && POS(sentence, question)

    def slidingWindow(subtree: Sentence): List[Sentence] = {
      /**
       * use a sliding window on the SUBTREE in order to take all candidates
       */
      var out: List[Sentence] = List()

      for (i    <- 0 until subtree.length;
           size <- 1 to Math.min(subtree.length - i, getConfig("maximumWindow").toInt)) {

        // the candidate sentence
        val nextSentence = subtree.getPortion(i, i + size)

        /*
         * check if there is something between the sliding window and the topic
         * entity (the subtree): in that case skip the candidate; otherwise add
         * the candidate to the output list
         */
        if (check(nextSentence))
          if (nextSentence.id.head.toInt < topic.id.head.toInt) {
            val p0 = tree.id.indexOf(nextSentence.id.last)
            val p1 = tree.id.indexOf(topic.id.head)
            val infra = tree.getPortion(p0 + 1, p1)
            if (! POS(infra, question))
              out = out :+ nextSentence
          } else {
            val p0 = tree.id.indexOf(topic.id.last)
            val p1 = tree.id.indexOf(nextSentence.id.head)
            val infra = tree.getPortion(p0 + 1, p1)
            if (! POS(infra, question))
              out = out :+ nextSentence
          }
      }

      return out.toSet.toList.reverse
    }

    return subTrees.map(slidingWindow).toList.flatten
  }

  def cleanQuestion(question: String): String = {
    var out = question
    out = "[A-ÿ]+[\\,]?".r.findAllIn(question).mkString(" ")
    out = "(\\w+)".r.replaceAllIn(out, " $1 ")
    out = "\\s+".r.replaceAllIn(out, " ")
    return out.trim
  }


  def apply(question: String): (String, String) = {
    /**
     * an abstraction of the QA system: it does the whole process starting from
     * just a QUESTION
     */

    val logger = log()
    val tree: Sentence = Parser(cleanQuestion(question), logger)
    val sentenceGraphs: List[DUDES.SolutionGraph] = NEE(tree, logger)

    val solutionCandidate: Option[(DUDES.SolutionGraph, String)] =
      solve(sentenceGraphs, tree, logger)

    if (solutionCandidate.isDefined) {
      val solution = solutionCandidate.get
      return (solution._2, logger(solution._2))
    }

    val ans = "<!> ERROR: no answer for this question."
    return (ans, logger(ans))
  }

}


object PerfectMatch {

  def perfectMatch(candidate: String, possibilities: List[String]): Boolean = {
    /**
     * the match is considered perfect if the two strings (lowercased) are equal
     */
    val c = Lexicalization.cleanText(candidate).toLowerCase()
    return possibilities
      .exists(p => {
                val pClean = Lexicalization.cleanText(Lexicalization.cleanLabel(p))
                pClean.toLowerCase() == c
              })
  }

  /*
   * all the following functions have the same general structure: the
   * lexicalization for the category is taken, the list is processed to check
   * for perfect matches and (eventually) a new solution is given
   */
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
    /**
     * take all possible expansions for the perfect match and retrieve them
     */
    val out = getRelations(candidate, topic, lexicon, nextStepTree) ++
      getObjects(candidate, topic, lexicon, nextStepTree) ++
      getClasses(candidate, topic, lexicon, nextStepTree) ++
      getIncognitaClasses(candidate, topic, lexicon, nextStepTree)

    return out
  }

}

object FuzzyMatch {

  /*
   * again, the structure is the same, but this time the process is made using a
   * Doc2Vec HTTP server that retrieves a score -1 <= x <= +1
   */

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
    val classesIn = lexicon("class_incognita_in")
      .map(kv => {
             val newDudes = new DUDES.ClassIncognitaDUDES(sentence, kv._1,
                                                          topic.dist + 1, scores(kv._1))
             val edge = topic ~> newDudes
             candidate.addDUDES(edge)
           })
      .filter(g => g.isDefined)
      .map(g => g.get)
    val classesOut = lexicon("class_incognita_out")
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
    /**
     * exactly like for PerfectMatch, but using a more sophisticated lexicon (so 
     * that computing the cosine similarity is well optimized)
     * Since there is a score, retrieve only graphs that are verosimilar
     */
    val topicString = Lexicalization(topic, topic.sentence.lang)
    val out = (getRelations(candidate, topic, topicString, lexicon, nextStepTree) ++
                 getObjects(candidate, topic, topicString, lexicon, nextStepTree) ++
                 getClasses(candidate, topic, topicString, lexicon, nextStepTree) ++
                 getIncognitaClasses(candidate, topic, topicString, lexicon, nextStepTree))
    return out.filter(g => math.exp(g.score) > 0)
  }
}
