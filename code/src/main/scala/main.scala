package main


import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._

import scala.io.StdIn
import scala.util.parsing.json.JSONObject
import java.util.concurrent.TimeUnit


import semantics.QASystem


object Main {


  def startServer(): Unit = {
    /**
     * statr a simple REST server to accept questions
     */
    val name = constants.getConfig("name")
    implicit val system: ActorSystem = ActorSystem(name)
    implicit val executor = system.dispatcher

    val route =
      path("info") {
        get {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                              s"<h1>$name is online</h1>"))
        }
      } ~
      path("ask") {
        get {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                              s"<h1>Ask $name</h1>"))
        } ~
        post {
          entity(as[String]) { question =>        
            val startTime = System.nanoTime()
            val (ans, log) = QASystem(question)
            val deltaTime = TimeUnit.NANOSECONDS.toSeconds(System.nanoTime() - startTime)
            val out: Map[String, String] = Map(
              "question" -> question,
              "answere" -> ans,
              "log" -> log,
              "time" -> deltaTime.toString())
            complete(JSONObject(out).toString())
          }
        }
      }
    val port: Int = constants.getConfig("port").toInt
    val server = Http()
      .newServerAt("0.0.0.0", port)
      .bind(route)

    println(s"Server listening on port $port")
    
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
