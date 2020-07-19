package semantics


import org.apache.jena.query.QuerySolution


import nlp._
import semantics.NEE
import main.utils._


object QASystem {

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
    topic.candidates.foreach(topicCandidate => {
                               val candidate = uriToString(topicCandidate, "candidate")
                               println(candidate + ": " + Encoder("Titanic", candidate, question))
                             })
    return "La risposta è: 42"
  }


}
