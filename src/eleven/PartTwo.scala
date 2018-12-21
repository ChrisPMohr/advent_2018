package eleven

object PartTwo {
  val gridSize = 300

  def main(args: Array[String]) {
    val fuelCellBoundingBox = (1, 1, gridSize, gridSize)
    val fuelCellGrid = makeGrid(fuelCellBoundingBox)
    val fuelCells = fuelCellGrid.map(point => point -> getCellScore(point)).toMap

    val answerBoundingBox = (1, 1, gridSize, gridSize)
    val answerGrid = makeGrid(answerBoundingBox)
    val answers = answerGrid.map(point => getBestSquare(point, fuelCells))
    println(answers.maxBy(_._3))
  }

  def getBestSquare(point: Point, fuelCells: Map[Point, Int]): (Point, Int, Int) = {
    val biggestSquare = List(gridSize - point._1, gridSize - point._2, 50).min
    getBestSquareScore(point, 0, fuelCells, biggestSquare, 0)
  }

  def getBestSquareScore(point: Point, size: Int, fuelCells: Map[Point, Int], maxSize: Int, lastScore: Int): (Point, Int, Int) = {
    val newPoints =
      (0 to size).map(offset => (point._1 + size, point._2 + offset)) ++
        (0 to size - 1).map(offset => (point._1 + offset, point._2 + size))
    val score = lastScore + newPoints.map(fuelCells).sum
    if (size < maxSize) {
      val nextScore = getBestSquareScore(point, size + 1, fuelCells, maxSize, score)
      if (nextScore._3 > score) nextScore else (point, size, score)
    } else
      (point, size, score)
  }

  type BoundingBox = (Int, Int, Int, Int)
  type Point = (Int, Int)

  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }

  def makeGrid(boundingBox: BoundingBox): List[Point] = {
    ((boundingBox._1 to boundingBox._3) cross (boundingBox._2 to boundingBox._4)).toList
  }

  def getCellScore(point: Point): Int = {
    val serialNumber = 7315
    val rackId = point._1 + 10
    val fullPowerLevel = (rackId * point._2 + serialNumber) * rackId
    (fullPowerLevel/100 % 10) - 5
  }
}
