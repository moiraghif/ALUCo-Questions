package main

import main.constants._
import nlp.Sentence

object utils {

  def repeat(elem: Boolean, times: Int): Array[Boolean] =
    /**
     * repeat a boolean ELEM for TIMES times
     */
    (1 to times).map(i => elem).toArray


  def binaryPermutations(len: Int): Seq[Array[Boolean]] =
    /**
     * get all permutations of a binary vector of arbitrary LENght
     */
    for (i <- 0 to len;
       pi <- (repeat(false, i) ++ repeat(true, len - i)).permutations) yield pi


  def isATree(sentence: Sentence): Boolean = {
    /**
     * return if a SENTENCE is a valid tree syntactically speaking
     */
    val heads: Array[Int] = sentence.dep.map(
      d => {
        if (sentence.id.contains(d)) 0
        else 1
      })
    return heads.sum == 1
  }


  def isSubStringOf(substring: Sentence, string: Sentence): Boolean =
    /**
     * check if a SUBSTRING is really a substring of a STRING
     */
    ! substring.id.exists(! string.id.contains(_))


  def getTreeRoot(tree: Sentence): String = {
    /**
     * get the root element of a (sub)TREE
     */
    for (i <- 0 until tree.length)
      if (! tree.id.contains(tree.dep(i)))
        return tree.id(i)
    return ""
  }


  def splitIntoSubtrees(tree: Sentence): Array[Sentence] = {
    /**
     * split a TREE into subtrees, if syntax rules are not followed
     */
    if (tree.isValidTree)
      return Array[Sentence](tree)

    def hasAsHead(pos: Int): String = {
      val head = tree.id.indexOf(tree.dep(pos))
      if (head == -1) return tree.id(pos)
      return hasAsHead(head)
    }

    val heads = (0 until tree.length).map(hasAsHead)
    return heads.toSet.toArray.map(
      h => {
        val index = (0 until tree.length).filter(i => heads(i) == h)
        tree.get(index.toArray)
      })
  }
}
