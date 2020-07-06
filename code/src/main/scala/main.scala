package main


import semantics.QASystem
import semantics.KGfunctions._


import main.constants._


object Main {

  def startShell(): Unit = {
    // start a shell interface to the main program
    while (true) {
      print(">> ")
      val text: String = readLine()
      QASystem(text)
    }
  }

  def main(args: Array[String]) = {
    // main function: start all the program
    startShell()
    if (printLog) println("Bye! :)")
  }

}
