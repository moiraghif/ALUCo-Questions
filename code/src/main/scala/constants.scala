package main


import spray.json._
import DefaultJsonProtocol._
import scala.io.Source

object constants {

  val configFile = "config.json"

  def getConfig(field: String): String = {

    def readJSON(f: Array[String], json: String): String = {
      val clean = (w: String) => "(?<=^\").+(?=\"$)".r
        .findFirstIn(w)
        .getOrElse(w)
      val answer: String = json.parseJson.asJsObject
        .getFields(f(0))(0).toString
      if (f.length == 1) return clean(answer)
      return readJSON(f.tail, answer)
    }

    val configs = Source.fromFile(configFile)
      .getLines.mkString
    val fields = field.split("\\.")
    return readJSON(fields, configs)
  }


  def getServerURL: String = {
    // just get the clean URL of the server
    // (example): http://localhost:3030/QAsystem
    val clean = (w: String) =>
      if (s"${w.last}" == "/") w.substring(0, w.length - 1)
      else w
    val serverURL  = clean(getConfig("HTTP_triplestore.url"))
    val serverName = clean(getConfig("HTTP_triplestore.name"))
    return s"$serverURL/$serverName"
  }

  def printLog(): Boolean = {
    return getConfig("printLog") == "true"
  }

}
