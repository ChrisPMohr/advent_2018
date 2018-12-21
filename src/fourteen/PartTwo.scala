package fourteen

import scala.annotation.tailrec

object PartTwo {
  def main(args: Array[String]) {
    val scores = Vector(3, 7)
    val firstPos = 0
    val secondPos = 1

    println(getAnswer(scores, firstPos, secondPos, "286051"))
  }

  @tailrec
  def getAnswer(scores: Vector[Int], firstPos: Int, secondPos: Int, answerPattern: String): Int = {
    val answerSize = answerPattern.length
    if (scores.takeRight(answerSize).mkString("") == answerPattern)
      scores.size - answerSize
    else if (scores.dropRight(1).takeRight(answerSize).mkString("") == answerPattern)
      scores.size - 1 -answerSize
    else {
      val ret = nextScores(scores, firstPos, secondPos)
      getAnswer(ret._1, ret._2, ret._3, answerPattern)
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
