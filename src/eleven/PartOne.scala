package eleven

object PartOne {
  def main(args: Array[String]) {
    val fuelCellBoundingBox = (1, 1, 300, 300)
    val fuelCellGrid = makeGrid(fuelCellBoundingBox)
    val fuelCells = fuelCellGrid.map(point => point -> getCellScore(point)).toMap

    val answerBoundingBox = (1, 1, 298, 298)
    val answerGrid = makeGrid(answerBoundingBox)
    val answers = answerGrid.map(
      point => (point, makeGrid((0,0,2,2)).map(offset => fuelCells((point._1 + offset._1, point._2 + offset._2))).sum))
    println(answers.maxBy(_._2))
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
