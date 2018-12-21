package eight

object PartOne {
  def main(args: Array[String]) {
    val treeInput = scala.io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val tree: Tree = parseTree(treeInput.toIterator)
    println(sumTree(tree))
  }

  class Tree
  case class Node(nodes: List[Tree], metadata: List[Int]) extends Tree
  case class Leaf(metadata: List[Int]) extends Tree

  def parseTree(tree: Iterator[Int]): Tree = {
    def parseChild(tree:Iterator[Int], numChildren: Int): List[Tree] = {
      if (numChildren == 0)
        Nil
      else
        parseTree(tree)::parseChild(tree, numChildren-1)
    }
    val numChildren = tree.next()
    val numMetadata = tree.next()
    val children: List[Tree] = parseChild(tree, numChildren)
    val metadata: List[Int] = tree.take(numMetadata).toList
    if (children.nonEmpty)
      Node(children, metadata)
    else
      Leaf(metadata)
  }

  def sumTree(tree: Tree): Int = tree match {
    case Leaf(metadata: List[Int]) => metadata.sum
    case Node(children, metadata) => children.map(child => sumTree(child)).sum + metadata.sum
  }
}
