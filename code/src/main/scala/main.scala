package main


import semantics.QASystem
import scala.io.StdIn


object Main {

  def startShell(): Unit = {
    // start a shell interface to the main program
    QASystem("Who is the director of Titanic with Leonardo DiCaprio ?")
    return
    while (true) {
      print(">> ")
      val text = StdIn.readLine().trim
      if (text == ":exit") return
      else QASystem(text)
    }
  }

  def main(args: Array[String]) = {
    // main function: start all the program
    startShell()
  }

}
