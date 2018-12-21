package ten
import math.abs
import scala.annotation.tailrec

object PartOne {
  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines.toList
    val points = lines.map(parsePoint)
    val closePoints = getClosePoints(points)
    val boundingBox = getBoundingBox(closePoints)
    printPoints(closePoints, boundingBox)
  }

  type Point = (Int, Int, Int, Int)
  type BoundingBox = (Int, Int, Int, Int)

  def parsePoint(line: String): Point = {
    val pattern = "position=< *(-?[0-9]+),  *(-?[0-9]+)> velocity=< *(-?[0-9]+),  *(-?[0-9]+)>".r
    val pattern(x, y, dx, dy) = line
    (x.toInt, y.toInt, dx.toInt, dy.toInt)
  }

  def getBoundingBox(points: List[Point]): BoundingBox = {
    (points.minBy(_._1)._1, points.minBy(_._2)._2,
      points.maxBy(_._1)._1, points.maxBy(_._2)._2)
  }

  def printPoints(points: List[Point], boundingBox: BoundingBox): Unit = {
    val pointMap = points.map(point => (point._1, point._2) -> true).toMap
    def printRow(row: Int): Unit = {
      println((boundingBox._1 to boundingBox._3).map(x => {
        if (pointMap.getOrElse((x, row), false))
          '#'
        else '.'
      }).mkString(""))
      if (row < boundingBox._4)
        printRow(row + 1)
    }
    printRow(boundingBox._2)
  }

  def doStep(points: List[Point]): List[Point] = {
    points.map(point => (point._1 + point._3, point._2 + point._4, point._3, point._4))
  }

  def arePointsClose(points: List[Point]): Boolean = {
    def closestDistances = points.map(
      point => points
        .filter(point2 => (point._1 != point2._1) || (point._2 != point2._2))
        .map(point2 => manhattanDistance(point, point2)).min
    )
    closestDistances.sum < (points.size * 1.1)
  }

  def manhattanDistance(p1: Point, p2: Point): Int = {
    abs(p1._1 - p2._1) + abs(p1._2 - p2._2)
  }

  @tailrec
  def getClosePoints(points: List[Point]): List[Point] = {
    val newPoints = doStep(points)
    if (arePointsClose(newPoints))
      newPoints
    else
      getClosePoints(newPoints)
  }
}
