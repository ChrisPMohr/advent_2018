package fourteen

import scala.annotation.tailrec

object PartOne {
  def main(args: Array[String]) {
    val scores = Vector(3, 7)
    val firstPos = 0
    val secondPos = 1

    println(getAnswer(scores, firstPos, secondPos, 286051).mkString(""))
  }

  @tailrec
  def getAnswer(scores: Vector[Int], firstPos: Int, secondPos: Int, toDrop: Int): Vector[Int] = {
    if (scores.size >= toDrop + 10)
      scores.drop(toDrop).take(10)
    else {
      val ret = nextScores(scores, firstPos, secondPos)
      getAnswer(ret._1, ret._2, ret._3, toDrop)
    }
  }

  def nextScores(scores: Vector[Int], firstPos: Int, secondPos: Int): (Vector[Int], Int, Int) = {
    val firstScore = scores(firstPos)
    val secondScore = scores(secondPos)
    val firstNewPos = (firstScore + 1 + firstPos) % scores.size
    val secondNewPos = (secondScore + 1 + secondPos) % scores.size
    val scoreSum = scores(firstNewPos) + scores(secondNewPos)
    if (scoreSum >= 10)
      (scores :+ 1 :+ (scoreSum - 10), firstNewPos, secondNewPos)
    else
      (scores :+ scoreSum, firstNewPos, secondNewPos)
  }
}
