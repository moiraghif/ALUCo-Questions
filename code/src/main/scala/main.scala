package main


import semantics.QASystem
import scala.io.StdIn


object Main {

  // data to compare:
  // https://github.com/ag-sc/QALD
  def printSolution(question: String): Unit = {
    val solution = QASystem(question)
    println(s"\n => \n$solution")
  }
  def startShell(): Unit = {
    // start a shell interface to the main program
    return printSolution("Who directed Titanic ?")
    while (true) {
      print(">> ")
      val text = StdIn.readLine().trim
      if (text == ":exit") return
      else printSolution(text)  // QASystem(text)
    }
  }

  def main(args: Array[String]) = {
    // main function: start all the program
    startShell()
  }

}
