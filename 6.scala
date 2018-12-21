import math.abs

object DaySixPartOne {
    def main(args: Array[String]) {
        val lines = io.Source.stdin.getLines.toList
        val coords = lines.map(parseCoord)
        val boundingBox = getBoundingBox(coords)
        val grid = makeGrid(boundingBox)
        val closestPoints = getClosestPoints(grid, coords)
        val numClosestByIndex = closestPoints.groupBy(_._2).mapValues(_.size)
        val unqualifiedIndexes = getUnqualifiedIndexes(closestPoints, boundingBox)
        val numClosestByQualifiedIndex = numClosestByIndex.filter(kv => !unqualifiedIndexes(kv._1))
        println(numClosestByQualifiedIndex.maxBy(_._2)._2)
    }
    
    type Coord = (Int, Int)
    type BoundingBox = (Int, Int, Int, Int)
    
    def parseCoord(line: String): Coord = {
        val pattern = "([0-9]+), ([0-9]+)".r
        val pattern(x, y) = line
        (x.toInt, y.toInt)
    }
    
    def getBoundingBox(coords: List[Coord]): BoundingBox = {
        (coords.minBy(_._1)._1, coords.minBy(_._2)._2,
         coords.maxBy(_._1)._1, coords.maxBy(_._2)._2)
    }

    implicit class Crossable[X](xs: Traversable[X]) {
        def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
    }

    def makeGrid(boundingBox: BoundingBox) = {
        ((boundingBox._1 to boundingBox._3) cross (boundingBox._2 to boundingBox._4)).toList
    }
    
    def getClosestPoints(grid: List[Coord], coords: List[Coord]): Map[Coord, Int] = {
        val coordsWithIndex = coords.zipWithIndex
        grid.map(gridCoord =>
            gridCoord -> 
                getClosestPoint(coordsWithIndex.map(coordWithIndex => 
                    coordWithIndex._2 -> manhattanDistance(coordWithIndex._1, gridCoord)).toMap)
        ).toMap
    }
    
    def manhattanDistance(c1: Coord, c2: Coord) = {
        abs(c1._1 - c2._1) + abs(c1._2 - c2._2)
    }
    
    def getClosestPoint(distances: Map[Int, Int]): Int = {
        getBestOrTie(distances.groupBy(_._2).minBy(_._1)._2.keys.toList)
    }
    
    def getBestOrTie(options: List[Int]) = if (options.size == 1) options(0) else -1
    
    def getUnqualifiedIndexes(closestPoints: Map[Coord, Int], boundingBox: BoundingBox): Set[Int] = {
        closestPoints.filter(kv => isOnBoundary(kv._1, boundingBox)).values.toSet
    }
    
    def isOnBoundary(c: Coord, boundingBox: BoundingBox): Boolean = {
        c._1 == boundingBox._1 || c._1 == boundingBox._3 || c._2 == boundingBox._2 || c._2 == boundingBox._4
    }
}

import math.abs

object DaySixPartTwo {
    def main(args: Array[String]) {
        val lines = io.Source.stdin.getLines.toList
        val coords = lines.map(parseCoord)
        val boundingBox = getBoundingBox(coords)
        val grid = makeGrid(boundingBox)
        val gridWithTotalDistance = addTotalDistance(grid, coords)
        val pointsInRange = gridWithTotalDistance.filter(kv => kv._2 < 10000).keys
        println(pointsInRange.size)
    }
    
    type Coord = (Int, Int)
    type BoundingBox = (Int, Int, Int, Int)
    
    def parseCoord(line: String): Coord = {
        val pattern = "([0-9]+), ([0-9]+)".r
        val pattern(x, y) = line
        (x.toInt, y.toInt)
    }
    
    def getBoundingBox(coords: List[Coord]): BoundingBox = {
        (coords.minBy(_._1)._1, coords.minBy(_._2)._2,
         coords.maxBy(_._1)._1, coords.maxBy(_._2)._2)
    }

    implicit class Crossable[X](xs: Traversable[X]) {
        def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
    }

    def makeGrid(boundingBox: BoundingBox) = {
        ((boundingBox._1 to boundingBox._3) cross (boundingBox._2 to boundingBox._4)).toList
    }
    
    def addTotalDistance(grid: List[Coord], coords: List[Coord]): Map[Coord, Int] = {
        grid.map(gridCoord =>
            gridCoord -> getDistanceSum(gridCoord, coords)
        ).toMap
    }
    
    def getDistanceSum(coord: Coord, coords: List[Coord]): Int = {
        coords.map(c => manhattanDistance(c, coord)).sum
    }
    
    def manhattanDistance(c1: Coord, c2: Coord) = {
        abs(c1._1 - c2._1) + abs(c1._2 - c2._2)
    }
}