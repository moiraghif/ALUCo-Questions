package main


import nlp._
import semantics.QASystem
import semantics.KGfunctions._


import main.constants._


object Main {

  val encoder = new Encoder
  val parser  = new Parser


  def startShell(): Unit = {
    // start a shell interface to the main program
    while (true) {
      print(">> ")
      val text: String = readLine()
      QASystem(encoder, parser, text)
    }
  }

  def main(args: Array[String]) = {
    // main function: start all the program
    startShell()
    if (printLog) println("Bye! :)")
  }

}
