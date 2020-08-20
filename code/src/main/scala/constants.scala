package main


import spray.json._
import DefaultJsonProtocol._
import scala.io.Source

object constants {

  val configFile = "config.json"

  def getConfig(field: String): String = {
    /**
     * read a FIELD of the config json file; to read a sub-field just split with
     * dots (e.g. HTTP_triplestore.latency)
     */
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
      .getLines().mkString
    val fields = field.split("\\.")

    return readJSON(fields, configs)
  }


  def getLatency(): Float =
    /**
     * get the latency of queries in seconds, treat like a constant
     */
    getConfig("HTTP_triplestore.latency").toFloat * 1000


  def getServerURL: String = {
    /**
     * get the triplestore url
     */
    val clean = (w: String) =>
      if (s"${w.last}" == "/") w.substring(0, w.length - 1)
      else w
    val serverURL  = clean(getConfig("HTTP_triplestore.url"))
    val serverName = clean(getConfig("HTTP_triplestore.name"))
    return s"$serverURL/$serverName"
  }

  def printLog(): Boolean =
    /**
     * print debugging messages?
     */
    getConfig("printLog") == "true"

}
