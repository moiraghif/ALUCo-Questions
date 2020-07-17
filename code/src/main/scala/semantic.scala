package semantics


import nlp._
// import semantics.NEE


object QASystem {

  def apply(encoder: Encoder, parser: Parser, question: String): String = {
    println(encoder(question, "hello world"))
    val tree: Sentence = parser(question)
    // val topic: Array[QuerySolution] = NEE(tree)
    return "La risposta è: 42"
  }
}
