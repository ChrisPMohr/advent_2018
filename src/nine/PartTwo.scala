package nine

import scala.collection.mutable

object PartTwo {
  def main(args: Array[String]) {
    val numPlayers = 439
    val highestMarble = 7130800
    val marbles = new DoublyLinkedList()
    val marblesToInsert = 0 to highestMarble
    val playerStream = Stream.continually((1 to numPlayers).toStream).flatten
    val scores = mutable.HashMap.empty[Int, BigInt]
    (marblesToInsert zip playerStream).foreach(
      marbleAndPlayer => insertMarble(marbles, scores, marbleAndPlayer._1, marbleAndPlayer._2))
    println(scores.maxBy(_._2)._2)
  }

  abstract class Node {
    def print()
  }
  case class NonEmpty(elem: BigInt, var next: Node, var prev: Node) extends Node {
    def print(): Unit = {
      Predef.print(s"($elem)")
    }
  }
  case class Empty() extends Node {
    def print(): Unit = {
      Predef.print("()")
    }
  }

  val EMPTY = Empty()

  class DoublyLinkedList {
    var head: Node = EMPTY
    var last: Node = head

    def insertBeforeHead(node: NonEmpty): Unit = {
      node.prev = EMPTY
      head match {
        case headNode: NonEmpty =>
          node.next = head
          headNode.prev = node
          head = node
        case _: Empty =>
          node.next = EMPTY
          head = node
          last = node
      }
    }

    def insertAfterLast(node: NonEmpty): Unit = {
      node.next = EMPTY
      last match {
        case lastNode: NonEmpty =>
          node.prev = last
          lastNode.next = node
          last = node
        case empty: Empty =>
          node.prev = EMPTY
          last = node
          head = node
      }
    }

    def print(): Unit = {
      def printRecursive(node: Node): Unit = node match {
        case nonEmpty: NonEmpty => {
          node.print()
          Predef.print("=>")
          printRecursive(nonEmpty.next)
        }
        case _: Empty => node.print()
      }

      printRecursive(head)
      println()
    }

    def cycleForward(): Unit = head match {
      case _: Empty =>
      case headNode: NonEmpty =>
        headNode.next match {
          case nextHeadNode: NonEmpty =>
            nextHeadNode.prev = EMPTY
            insertAfterLast(headNode)
            head = nextHeadNode
          case _: Empty =>
        }
    }

    def cycleBackwards(): Unit = last match {
      case _: Empty =>
      case lastNode: NonEmpty =>
        lastNode.prev match {
          case nextLastNode: NonEmpty =>
            nextLastNode.next = EMPTY
            insertBeforeHead(lastNode)
            last = nextLastNode
          case _: Empty =>
        }
    }

    def removeHead(): Unit = head match {
      case _: Empty =>
      case headNode: NonEmpty =>
        head = headNode.next
        headNode.next match {
          case _: Empty =>
            last = EMPTY
          case nextHeadNode: NonEmpty =>
            nextHeadNode.prev = EMPTY
        }
    }
  }

  def insertMarble(marbles: DoublyLinkedList, scores: mutable.HashMap[Int, BigInt], marble: BigInt, player: Int): Unit = {
    if (marble > 0 && marble % 23 == 0) {
      val oldScore: BigInt = scores.getOrElse(player, 0)
      marbles.cycleBackwards()
      marbles.cycleBackwards()
      marbles.cycleBackwards()
      marbles.cycleBackwards()
      marbles.cycleBackwards()
      marbles.cycleBackwards()
      marbles.cycleBackwards()
      marbles.head match {
        case _: Empty =>
        case headNode: NonEmpty =>
          marbles.removeHead()
          scores.update(player, oldScore + marble + headNode.elem)
      }
    } else {
      val newNode = NonEmpty(marble, EMPTY, EMPTY)
      marbles.cycleForward()
      marbles.cycleForward()
      marbles.insertBeforeHead(newNode)
    }
  }
}
