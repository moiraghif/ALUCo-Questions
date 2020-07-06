package main

import main.constants._

object utils {

  def repeat(elem: Boolean, times: Int): Array[Boolean] =
    // repeat a boolean ELEM for TIMES times
    (1 to times).map(i => elem).toArray


  def binaryPermutations(len: Int): Seq[Array[Boolean]] =
    // get all permutations of a binary vector of arbitrary LENght
    for (i <- 0 to len;
       pi <- (repeat(false, i) ++ repeat(true, len - i)).permutations) yield pi

  def isATree(id: Array[String], deps: Array[String]): Boolean = {
    // return if a piece of text is a valid tree
    val heads: Array[Int] = deps.map(
      d => {
        if (id.contains(d)) 0
        else 1
      })
    return heads.sum == 1
  }


  def getTreeRoot(tree: Map[String, Array[String]]): String = {
    // get the root element of a (sub)TREE
    if (! isATree(tree("id"), tree("dep"))) return ""
    for (i <- 0 until tree("id").length)
      if (! tree("id").contains(tree("dep")(i)))
        return tree("id")(i)
    return ""
  }

  def getElemDepth(elem: String, tree: Map[String, Array[String]]): Int = {
    // get the depth of an ELEMent in a (sub)TREE
    val root = getTreeRoot(tree)
    if (root.isEmpty) return -1
    def step(e: String, n: Int): Int = {
      if (e == root) return n
      else {
        val p = tree("id").indexOf(e)
        step(tree("dep")(p), n + 1)
      }
    }
    return step(elem, 0)
  }

  def getTreeMaxDepth(subtree: Map[String, Array[String]], tree: Map[String, Array[String]]): Int =
    // get the maximum depth of a (SUB)TREE
    tree("id")
      .filter(subtree("id").contains(_))
      .map(getElemDepth(_, tree))
      .max


  def getTreeMinDepth(subtree: Map[String, Array[String]], tree: Map[String, Array[String]]): Int =
    // get the minimum depth of a (SUB)TREE
    tree("id")
      .filter(subtree("id").contains(_))
      .map(getElemDepth(_, tree))
      .min
}
