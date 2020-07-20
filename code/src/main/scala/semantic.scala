package semantics


import org.apache.jena.query.QuerySolution


import nlp._
import semantics.NEE
import main.utils._
import semantics.KGfunctions._


class SolutionGraph(val before: Sentence,
                    val graph: String,
                    val after: Sentence)


object QASystem {

  def getCandidates(uri: QuerySolution, variable: String): Array[QuerySolution] = {
    val topic: String = "<" + uri.get("?" + variable).toString + ">"
    val query = s"""SELECT DISTINCT ?candidate ?label WHERE {
                    |  {          # relations
                    |    ${topic}   ?candidate  ?x.
                    |    ?candidate rdfs:label  ?label.
                    |  } UNION {  # entities
                    |    ${topic}   ?r          ?candidate.
                    |    ?candidate rdfs:label  ?label.
                    |  } UNION {  # classes
                    |    ${topic}   ?r          ?x.
                    |    ?x         rdf:isA     ?candidate.
                    |    ?candidate rdfs:label  ?label.
                    |  } UNION {  # predicates
                    |    ${topic}   ?r1         ?x.
                    |    ?x         ?r2         ?candidate.
                    |    ?candidate rdfs:label  ?label.
                    |  }
                    |  FILTER(lang(?label) = "en")
                    |}""".stripMargin
    val candidates = querySelect(query)
    return candidates.toArray
  }

  def filterCandidates(candidates: Array[QuerySolution], question: String): Double = {
    val getLabel = (candidate: QuerySolution)=> {
      val languageRegex = "@\\w{2}$".r
      val rawString = candidate.get("?label").toString
      languageRegex.replaceFirstIn(rawString, "")
    }
    Encoder(candidates.map(getLabel), "Titanic", "Leonardo Di Caprio")
    return 0.5
  }


  def uriToString(uri: QuerySolution, variable: String): String =
    uri.get("?" + variable).toString

  def exploreRelations(tree: Sentence, topic: Sentence): List[String] = {
    val headQuestion = getTreeRoot(tree)
    val headTopic = getTreeRoot(topic)
    val headTopicPosition = tree.id.indexOf(headTopic)
    def getNext(acc: List[String]): List[String] = {
      if (acc.last == headQuestion) return acc
      return getNext(acc :+ tree.dep(acc.last.toInt - 1)) 
    }
    val subTrees = getNext(List[String](tree.dep(headTopicPosition)))
    val candidates = tree.text(subTrees.head.toInt - 1) +: subTrees.tail.map(
      t => {
        val p0 = subTrees(0).toInt - 1
        val p1 = t.toInt - 1
        if (p0 < p1) tree.text.slice(p0, p1 + 1).mkString(" ")
        else         tree.text.slice(p1, p0 + 1).mkString(" ")
      })
    return candidates
  }

  def apply(question: String): String = {
    val tree: Sentence = Parser(question)
    val topic: Sentence = NEE(tree)
    // topic.candidates.foreach(topicCandidate => {
    //                            val candidate = uriToString(topicCandidate, "candidate")
    //                            println(candidate + ": " + Encoder("Titanic", candidate, question))
    //                          })
    return "La risposta è: 42"
  }


}
