package semantics

import nlp.Parser

object QASystem {
  def apply(question: String): String = {
    Parser(question)
    return "42"
  }
}
