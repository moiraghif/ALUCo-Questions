package main

import main.constants._
import nlp.Sentence

object utils {

  def repeat(elem: Boolean, times: Int): Array[Boolean] =
    // repeat a boolean ELEM for TIMES times
    (1 to times).map(i => elem).toArray


  def binaryPermutations(len: Int): Seq[Array[Boolean]] =
    // get all permutations of a binary vector of arbitrary LENght
    for (i <- 0 to len;
       pi <- (repeat(false, i) ++ repeat(true, len - i)).permutations) yield pi

  def isATree(sentence: Sentence): Boolean = {
    // return if a piece of text is a valid tree
    val heads: Array[Int] = sentence.dep.map(
      d => {
        if (sentence.id.contains(d)) 0
        else 1
      })
    return heads.sum == 1
  }

  def isSubStringOf(substring: Sentence, string: Sentence): Boolean =
    ! substring.id.exists(! string.id.contains(_))


  def getTreeRoot(tree: Sentence): String = {
    // get the root element of a (sub)TREE
    for (i <- 0 until tree.length)
      if (! tree.id.contains(tree.dep(i)))
        return tree.id(i)
    return ""
  }

  def getElemDepth(elem: String, tree: Sentence): Int = {
    // get the depth of an ELEMent in a (sub)TREE
    val root = getTreeRoot(tree)
    if (root.isEmpty) return -1
    def step(e: String, n: Int): Int = {
      if (e == root) return n
      else {
        val p = tree.id.indexOf(e)
        step(tree.dep(p), n + 1)
      }
    }
    return step(elem, 0)
  }

  def getTreeMaxDepth(subtree: Sentence, tree: Sentence): Int =
    // get the maximum depth of a (SUB)TREE
    tree.id
      .filter(subtree.id.contains(_))
      .map(getElemDepth(_, tree))
      .max


  def getTreeMinDepth(subtree: Sentence, tree: Sentence): Int =
    // get the minimum depth of a (SUB)TREE
    tree.id
      .filter(subtree.id.contains(_))
      .map(getElemDepth(_, tree))
      .min

  def splitIntoSubtrees(tree: Sentence): Array[Sentence] = {
    if (tree.isValidTree) return Array[Sentence](tree)
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
    // val pos = heads.toSet.toArray.map(h => heads.indexOf(h)) :+ heads.length
    // val positions = (0 until (pos.length - 1)).map(i => (pos(i), pos(i + 1)))
    // return positions.map(tree.getPortion).toArray
  }
}
