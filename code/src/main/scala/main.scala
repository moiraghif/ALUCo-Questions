package main


import semantics.QASystem
import scala.io.StdIn


object Main {

  def startShell(): Unit = {
    // start a shell interface to the main program
    while (true) {
      print(">> ")
      QASystem(StdIn.readLine())
    }
  }

  def main(args: Array[String]) = {
    // main function: start all the program
    startShell()
  }

}
