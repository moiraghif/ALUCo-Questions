package nlp

import scala.collection.JavaConverters._
import collection.mutable
import scala.util.matching.Regex

import org.apache.tika.langdetect.OptimaizeLangDetector
import cz.cuni.mff.ufal.udpipe.{
  Model,
  Pipeline }

import main.constants._
import semantics.NEE


object Parser {

  // the model used for each (supported) language
  val modelsList = getModelsPath()
  val models = getLazyModel(modelsList)


  def getModelsPath(): Map[String, String] = {
    val path: String = getConfig("udpipe.directory")
    return getConfig("udpipe.models")
      // get all the models
      .split(",")
      // clean the text
      .map(c =>
        c.trim
          .stripSuffix("}")
          .stripPrefix("{")
          .trim)
      // get all the tuples <language, filename>
      .map(conf =>
        "^\"([a-z]{2,3})\"\\s*:\\s*\"(.+)\"$".r
          .findAllIn(conf).matchData.map(
            l => {
              (l.group(1).toString, path + "/" + l.group(2).toString)
            }).toArray)
      // get only the tuple, not as list
      .map(l => l(0))
      .toMap
  }

  def getLanguage(text: String): String = {
    // get the language of a piece of TEXT, the result is in ISO-639 format
    // (https://www.loc.gov/standards/iso639-2/php/code_list.php)
    val detector = new OptimaizeLangDetector().loadModels(modelsList.keySet.asJava)
    val out = detector.detect(text)
    if (printLog) println(out.getLanguage() + ": " + out.getRawScore() + "")
    return out.getLanguage()
  }

  def loadModel(language: String): Model = {
    // select the udpipe model for a specific LANGUAGE
    val model = modelsList(language)
    if (printLog) println("Loading model: " + model + " ...")
    return Model.load(model) 
  }

  def getLazyModel(models: Map[String, String]): (String) => Model = {
    // get lazily a model from a list of MODELS for a specific LANGUAGE
    var loadedModels = mutable.Map[String, Model]()
    return (language: String) => {
      if (! loadedModels.contains(language))
        loadedModels += (language -> loadModel(language))
      loadedModels(language)
    }
  }


  def getTree(text: String, language: String): Map[String, Array[String]] = {
    // use a udipipe model to get a tree representation of a TEXT
    //
    // load the model for the specific language
    val pipeline = new Pipeline(models(language),
                                "horizontal",
                                Pipeline.getDEFAULT(),
                                Pipeline.getDEFAULT(),
                                "conllu")
    // parse the sentence and parse the output
    val parsed_sent: String = pipeline.process(text)
    val isComment = (line: String) => line matches "^#.*|"
    // the output is stored as a table, where column DEP represent the
    // dependency link (ID of the head)
    var out: mutable.HashMap[String, Array[String]] = new mutable.HashMap[String, Array[String]]()
    val outColumns = Array("id", "text", "lemma", "pos", "flex", "dep", "link")
    for (c <- outColumns) { out += (c -> Array()) }
    parsed_sent.split("\n").filter(x => !isComment(x)).foreach(
      line => {
        val parserLine = ("^(\\d+)\\t" +   // id
                           "(\\S+)\\t" +   // text
                           "(\\S+)\\t" +   // lemma
                           "(\\S+)\\t" +   // pos tagging
                           "\\S+\\t"   +   // other (useless) info
                           "(\\S+)\\t" +   // information about noun and verbs
                           "(\\S+)\\t" +   // header of the word
                           "(\\S+).*$").r  // link type with header
        parserLine.findAllIn(line).matchData.foreach(
          parsedLine =>
            for ((column, i) <- outColumns.zipWithIndex)
              out(column) = out(column) :+ parsedLine.group(i + 1))
      })
    return out.toMap
  }

  def printTreeAsTable(tree: Map[String, Array[String]]): Unit = {
    // just print the table of dependencies (TREE) as a table
    val columns = Array("id", "text", "lemma", "pos", "flex", "dep", "link")
    val lengthMax = columns.map(c => tree(c).map(r => r.length).max)
    val printLine = (i: Int) => {
      val line = columns.map(c => tree(c)(i))
      print("| ")
      (0 until line.length).foreach(i =>
        print(line(i) + " " * (lengthMax(i) - line(i).length) + " | "))
      print("\n")
    }
    (0 until tree("id").length).foreach(i => printLine(i))
  }

  def apply(text: String): Map[String, Array[String]] = {
    // one function to rule all: it takes a TEXT and returns the parsed sentences
    val lang = getLanguage(text)
    val tree = getTree(text, lang)
    if (printLog) printTreeAsTable(tree)
    for (s <- NEE.slidingWindow(tree, lang))
      println(s)
    return tree
  }
}
