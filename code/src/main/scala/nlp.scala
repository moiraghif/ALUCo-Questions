package nlp


import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex
import scalaj.http.Http
import collection.mutable

import org.apache.http.client.methods.HttpPost
import org.apache.http.message.BasicNameValuePair

import org.apache.tika.langdetect.OptimaizeLangDetector
import cz.cuni.mff.ufal.udpipe.{
  Model,
  Pipeline }
import org.apache.jena.query.QuerySolution
import org.apache.jena.rdf.model.RDFNode
import spray.json._
import DefaultJsonProtocol._


import main.utils._
import main.constants._


class Sentence(tree: Map[String, Array[String]], val lang: String) {
  /**
   * This data structure stores information about the sentence and permits to
   * easily extract a sub-tree from it
   */
  val id: Array[String]    = tree("id")
  val text: Array[String]  = tree("text")
  val lemma: Array[String] = tree("lemma")
  val pos: Array[String]   = tree("pos")
  val flex: Array[String]  = tree("flex")
  val dep: Array[String]   = tree("dep")
  val link: Array[String]  = tree("link")

  val sentence: String = text.mkString(" ")

  val length: Int = id.length
  val isValidTree: Boolean = isATree(this)


  def get(index: Array[Int]): Sentence =
    /**
     * get a portion of sentence according to a INDEX
     */
    new Sentence(
      Map[String, Array[String]](
        "id"    -> index.map(id),
        "text"  -> index.map(text),
        "lemma" -> index.map(lemma),
        "pos"   -> index.map(pos),
        "flex"  -> index.map(flex),
        "dep"   -> index.map(dep),
        "link"  -> index.map(link)),
      lang)
  def get(index: Array[String]): Sentence = get(index.map(i => id.indexOf(i)))
  def get(id: String): Sentence = get(Array(id))

  def getPortion(portion: (Int, Int)): Sentence = get((portion._1 until portion._2).toArray)

  def remove(index: Array[Int]): Sentence = get((0 until length).toArray.filter(! index.contains(_)))
  def remove(index: Array[String]): Sentence = remove(index.map(id.indexOf(_)))


  def getTree(): Map[String, Array[String]] =
    /**
     * returns a copy of the tree of the sentence
     */
    Map[String, Array[String]](
      "id"    -> id,
      "text"  -> text,
      "lemma" -> lemma,
      "pos"   -> pos,
      "flex"  -> flex,
      "dep"   -> dep,
      "link"  -> link)

  def + (other: Sentence): Sentence = {
    val sentences: Array[Sentence] = (id.map(i => get(i)) ++
      other.id.map(i => other.get(i))).sortBy(s => s.id.head.toInt) 
    val tree: Map[String, Array[String]] = getTree().keySet.map(k => {
                                                        (k, sentences.map(s => s.getTree()(k)).flatten)
                                                      }).toMap[String, Array[String]]
    new Sentence(tree, lang)
  }

  def toTable(): String = {
    /**
     * print the Sentence as a table
     */
    val columns: Array[Array[String]] = Array(id, text, lemma, pos, flex, dep, link)
    val lengthMax = columns.map(c => c.map(r => r.length).max)
    val rows: Int = length
    val cols: Int = columns.length
    return (0 until rows).toArray.map(r =>
      "| " +
        (0 until cols).map(c => {
                             val text = columns(c)(r)
                             val spaces = lengthMax(c) - text.length
                             text + " " * spaces
                           }).mkString(" | ") +
        " |").mkString("\n")
  }

  override def toString(): String = sentence
}



object Parser {

  // the model used for each (supported) language
  val modelsList = getModelsPath()
  val models = getLazyModel(modelsList)


  def getModelsPath(): Map[String, String] = {
    /**
     * get the path for each model 
     */
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

  def getLanguage(text: String, logger: (String)=>String): String = {
    /**
     * get the language of a TEXT (the result is in ISO-639 format)
     * https://www.loc.gov/standards/iso639-2/php/code_list.php
     */
    val detector = new OptimaizeLangDetector().loadModels(modelsList.keySet.asJava)
    val out = detector.detect(text)
    logger(out.getLanguage() + ": " + out.getRawScore() + "")
    return out.getLanguage()
  }

  def loadModel(language: String): Model = {
    /**
     * select the right udpipe model for the LANGUAGE
     */
    val model = modelsList(language)
    return Model.load(model) 
  }

  def getLazyModel(models: Map[String, String]): (String) => Model = {
    /**
     * get lazily the models from a list of MODELS (annotated with language)
     * loading each one only when needed
     */
    var loadedModels = mutable.Map[String, Model]()
    return (language: String) => {
      if (! loadedModels.contains(language))
        loadedModels += (language -> loadModel(language))
      loadedModels(language)
    }
  }

  def getTree(text: String, language: String, logger: (String)=>String): Sentence = {
    /**
     * use a udpipe model to get a tree representation for a TEXT in a specified LANGUAGE
     */
    val pipeline = new Pipeline(models(language),  // load the model for the specific language
                                "horizontal",
                                Pipeline.getDEFAULT(),
                                Pipeline.getDEFAULT(),
                                "conllu")
    // parse the sentence and then parse the output
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
    return new Sentence(out.toMap, language)
  }

  def apply(text: String, logger: (String)=>String): Sentence = {
    /**
     * get a Sentence representation for the desired TEXT
     */
    val lang = getLanguage(text, logger)
    val tree = getTree(text, lang, logger)
    logger(tree.toTable())
    return tree
  }
}


object POS {
  /**
   * a more linguistic part
   */
  // from: http://universaldependencies.org/docs/u/pos/index.html
  val openPOS: Array[String]  = Array("ADJ", "ADV", "INTJ", "NOUN", "PROPN", "VERB")  // to add the IncognitaDUDES

  def openClassPOS(pos: String): Boolean = openPOS.contains(pos)
  def closedClassPOS(pos: String): Boolean = ! openClassPOS(pos)

  def isInterrogative(sentence: Sentence): Boolean =
    sentence.length == 1 && sentence.flex.head.contains("PronType=Int")


  def apply(sentence: Sentence): Boolean =
    sentence.pos.exists(word => openClassPOS(word)) || isInterrogative(sentence)
  def apply(subtree: Sentence, tree: Sentence): Boolean = {
    if (tree.link.filter(_ == "nsubj").length > 1)
      isInterrogative(subtree) || openPOS
        .filterNot(_ == "VERB")
        .exists(pos => subtree.pos.contains(pos))
    else apply(subtree)
  }
}


object Encoder {
  /**
   * make a REST request to a BERT server to get cosine similarity between a
   * piece of text and a list of candidates, eventually with a string to
   * contextualize the meaning (so that the attention mechanism works does a
   * better job)
   */
  def getScores(candidate: Map[RDFNode, List[String]],
                labels: List[String],
                substring: Sentence,
                sentence: String): Map[RDFNode, Double] = {
    /**
     * get the scores for a single batch
     */
    val jsonIn: String = s"""{
                            |    "substring": "${substring}",
                            |    "sentence": "${sentence}",
                            |    "candidates": """.stripMargin +
      "[" + labels.map(c => s""" "$c" """).mkString(",") + "]\n}"
    /* e.g. of input
     * {
     *   "substring": "substring to compare",
     *   "sentence": "a sentence to contextualize the substring",
     *   "candidates": [ "list", "of", "candidates" ]
     * }
     */
    val request = Http(getConfig("Doc2Vec.url"))
      .timeout(connTimeoutMs = 60 * 1000, readTimeoutMs = 5 * 60 * 1000)
      .header("content-type", "application/json")
      .postData(jsonIn)
      .asString
      .body
    try {
      val sim = request
        .parseJson
        .convertTo[Map[String, Double]]
      val out = candidate
        .map(kv =>
          (kv._1, kv._2.map(label => sim.getOrElse(label, -1.0)).max))
        .toMap[RDFNode, Double]
      return out
    } catch {
      case e: Throwable => {
        println(jsonIn)
        println("Exception occurred! server log:")
        println(request.toString)
        return candidate.map(kv => (kv._1, -1.0))
      }
    }
  }

  def apply(candidate: Map[RDFNode, List[String]],
            substring: Sentence,
            sentence: String): Map[RDFNode, Double] = {
    /**
     * split the lexicalization of CANDIDATEs into batches of size = 1000 and
     * pass them into a Doc2Vec model that analyzes the text and gives the
     * similarity score with a SUBSTRING, eventually using a SENTENCE to
     * contextualize
     */
    val out =  scala.collection.mutable.Map[RDFNode, Double]()
    candidate.keySet.foreach(node => out(node) = -1.0)

    val uniqueCandidates = candidate.map(kv => kv._2).flatten.toSet.toList
    val batchSize = 1000
    val n = (uniqueCandidates.length / batchSize.toDouble).ceil.toInt

    (0 until n).foreach(i => {
                          val limInf = i * batchSize
                          val limSup = math.min((i + 1) * batchSize, uniqueCandidates.length)
                          val batch = uniqueCandidates.slice(limInf, limSup)
                          val temp = getScores(candidate, batch, substring, sentence)
                          candidate.keySet.foreach(node =>
                            out(node) = out(node).max(temp(node)))
                        })
    return out.toMap
  }
}
