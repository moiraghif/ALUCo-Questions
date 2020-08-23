package main


import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import scala.io.StdIn


import semantics.QASystem


object Main {


  def startServer(): Unit = {
    /**
     * statr a simple REST server to accept questions
     */
    implicit val system: ActorSystem = ActorSystem("QA-System")
    implicit val executor = system.dispatcher
    val route =
      path("query") {
        post {
          entity(as[String]) { question =>
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                                QASystem(question)))
          }
        }
      }

    val port: Int = constants.getConfig("port").toInt
    val server = Http()
      .newServerAt("localhost", port)
      .bind(route)

    println(s"Server listening on port$port/\nPress RETURN to stop")
    StdIn.readLine()
    server
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }


  // data to compare:
  // https://github.com/ag-sc/QALD
  def printSolution(question: String): Unit = {
    val solution = QASystem(question)
    println(s"\n=> \n$solution")
  }


  def startShell(): Unit = {
    // start a shell interface to the main program
    // return printSolution("Which person is the director of the movie Titanic with Leonardo DiCaprio ?")
    while (true) {
      print(">> ")
      val text = StdIn.readLine().trim
      if (text == ":exit") return
      else printSolution(text)  // QASystem(text)
    }
  }

  def main(args: Array[String]) = {
    // main function: start all the program
    startServer()
  }

}
