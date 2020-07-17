package semantics


import nlp._
// import semantics.NEE


object QASystem {

  def apply(question: String): String = {
    val tree: Sentence = Parser(question)
    println(Encoder(question, "hello world"))
    // val topic: Array[QuerySolution] = NEE(tree)
    return "La risposta è: 42"
  }
}
