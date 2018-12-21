package eighteen

import scala.annotation.tailrec

object PartTwo {
  def main(args: Array[String]) {
    val lines: List[String] = io.Source.stdin.getLines.toList
    val linesWithIndex: List[(Char, (Int, Int))] = lines.zipWithIndex.flatMap(
      el => el._1.zipWithIndex.map(el2 => (el2._1, (el2._2, el._2)))
    )
    val landscape: Landscape = linesWithIndex
      .map(cAndPos => cAndPos._2 -> parseAcre(cAndPos._1))
      .toMap

    println(getFinalScore(landscape, 1000000000))
  }

  def getFinalScore(landscape: Landscape, lastStep: Int): Int = {
    _getFinalScore(landscape, Map(), Map(), 1, lastStep)
  }

  @tailrec
  def _getFinalScore(landscape: Landscape,
                     seenLandscapes: Map[Landscape, Int],
                     stepScores: Map[Int, Int],
                     step: Int,
                     lastStep: Int): Int = {
    val newLandscape: Landscape = doStep(landscape)
    val score = getScore(newLandscape)
    if (seenLandscapes.contains(newLandscape)) {
      val prevStep = seenLandscapes(newLandscape)
      val cycleLength = step - prevStep
      val remainder = (lastStep - prevStep) % cycleLength
      stepScores(prevStep + remainder)
    } else {
      _getFinalScore(
        newLandscape,
        seenLandscapes + (newLandscape -> step),
        stepScores + (step -> score),
        step + 1,
        lastStep)
    }
  }

  def getScore(landscape: Landscape): Int = {
    landscape.count(kv => kv._2 == Tree) * landscape.count(kv => kv._2 == Lumber)
  }

  sealed trait Acre

  case object Open extends Acre
  case object Tree extends Acre
  case object Lumber extends Acre

  type Pos = (Int, Int)
  type Landscape = Map[Pos, Acre]

  def parseAcre(c: Char): Acre = c match {
    case '.' => Open
    case '|' => Tree
    case '#' => Lumber
  }

  def printLandscape(landscape: Landscape): Unit = {
    println("-----------------------")
    val minX = landscape.keySet.minBy(_._1)._1
    val maxX = landscape.keySet.maxBy(_._1)._1
    val minY = landscape.keySet.minBy(_._2)._2
    val maxY = landscape.keySet.maxBy(_._2)._2
    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        val pos = (x, y)
        val char = landscape(pos) match {
          case Open => '.'
          case Tree => '|'
          case Lumber => '#'
        }
        print(char)
      }
      println("")
    }
    println("-----------------------")
  }

  def doStep(landscape: Landscape): Landscape = {
    landscape.keySet.map(k => k -> applyRules(k, landscape)).toMap
  }

  def applyRules(pos: Pos, landscape: Landscape): Acre = {
    val acre = landscape(pos)
    val neighborhood: List[Acre] = getNeighbors(pos).flatMap(landscape.get)
    acre match {
      case Open => if (neighborhood.count(a => a == Tree) >= 3) Tree else Open
      case Tree => if (neighborhood.count(a => a == Lumber) >= 3) Lumber else Tree
      case Lumber =>
        if (neighborhood.count(a => a == Tree) >= 1 && neighborhood.count(a => a == Lumber) >= 1)
          Lumber
        else
          Open
    }
  }

  def getNeighbors(pos: Pos): List[Pos] = {
    List(
      (pos._1 - 1, pos._2 - 1),
      (pos._1 - 1, pos._2),
      (pos._1 - 1, pos._2 + 1),
      (pos._1, pos._2 - 1),
      (pos._1, pos._2 + 1),
      (pos._1 + 1, pos._2 - 1),
      (pos._1 + 1, pos._2),
      (pos._1 + 1, pos._2 + 1),
    )
  }
}
