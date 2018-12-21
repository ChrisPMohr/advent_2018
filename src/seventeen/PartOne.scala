package seventeen

object PartOne {
  def main(args: Array[String]) {
    val lines: List[String] = io.Source.stdin.getLines.toList
    val clay: Set[Pos] = lines.flatMap(parseScan).toSet

    val spring: Pos = (500, 0)
    val unsettledWater: Set[Pos] = Set(spring)
    val settledWater: Set[Pos] = Set()

    def firstY: Int= clay.minBy(_._2)._2
    def lastY: Int= clay.maxBy(_._2)._2

    /*
    val partial = doStep(_: Set[Pos], _: Set[Pos], clay, lastY)
    val endState = (1 to 11)
        .foldLeft((unsettledWater, settledWater))
        {(v, i) => partial(v._1, v._2)}
        */

    val endState = repeatUntilDone(unsettledWater, settledWater, clay, lastY)
    //printState(endState._1, endState._2, clay)

    println(endState._1.union(endState._2).count(_._2 >= firstY))

    //println((endState._1.toList.sorted, endState._2.toList.sortBy(p => (p._2, p._1))))
  }

  type Pos = (Int, Int)
  type Scan = List[Pos]

  def printState(unsettledWater: Set[Pos], settledWater: Set[Pos], clay: Set[Pos]): Unit = {
    val filled  = unsettledWater.union(settledWater).union(clay)
    val minX = filled.minBy(_._1)._1
    val maxX = filled.maxBy(_._1)._1
    val minY = filled.minBy(_._2)._2
    val maxY = filled.maxBy(_._2)._2
    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        val pos = (x, y)
        if (unsettledWater.contains(pos)) {
          print('|')
        } else if (settledWater.contains(pos)) {
          print('~')
        } else if (clay.contains(pos)) {
          print('#')
        } else {
          print('.')
        }
      }
      println("")
    }
  }

  def repeatUntilDone(unsettledWater: Set[Pos],
                      settledWater: Set[Pos],
                      clay: Set[Pos],
                      lastY: Int): (Set[Pos], Set[Pos]) = {
    val result = doStep(unsettledWater, settledWater, clay, lastY)
    if (result._1 == unsettledWater)
      result
    else
     repeatUntilDone(result._1, result._2, clay, lastY)
  }


  def parseScan(line: String): Scan = {
    val scanPattern = raw"(.)=([0-9]+), (.)=([0-9]+)\.\.([0-9]+)".r
    val scanPattern(firstAxis, firstCoordStr, secondAxis, startStr, endStr) = line
    val firstCoord = firstCoordStr.toInt
    val start = startStr.toInt
    val end = endStr.toInt

    firstAxis match {
      case "x" => (start to end).map(y => (firstCoord, y)).toList
      case "y" => (start to end).map(x => (x, firstCoord)).toList
    }
  }

  def doStep(unsettledWater: Set[Pos],
             settledWater: Set[Pos],
             clay: Set[Pos],
             lastY: Int): (Set[Pos], Set[Pos]) = {
    val filled = clay.union(settledWater)
    val results: Set[(Set[Pos], Set[Pos])] = unsettledWater
      .map(source => tryToFillFromSource(source, unsettledWater, settledWater, clay, filled, lastY))
    val newUnsettled = results.map(_._1).fold(Set())({(s1, s2) => s1.union(s2)})
    val newSettled = results.map(_._2).fold(settledWater)({(s1, s2) => s1.union(s2)})
    (newUnsettled.diff(newSettled), newSettled)
  }

  def tryToFillFromSource(source: Pos,
                          unsettledWater: Set[Pos],
                          settledWater: Set[Pos],
                          clay: Set[Pos],
                          filled: Set[Pos],
                          lastY: Int) : (Set[Pos], Set[Pos]) = {
    if (source._2 + 1 > lastY) {
      (Set(), Set())
    } else {
      val posBelow = (source._1, source._2 + 1)
      val posLeft = (source._1 - 1, source._2)
      val posRight = (source._1 + 1, source._2)
      if (!(settledWater.contains(posBelow) || clay.contains(posBelow))) {
        (Set(source, posBelow), Set())
      } else if (
        (unsettledWater.contains(posLeft) || unsettledWater.contains(posRight)) &&
          unsettledWater.contains(posBelow)) {
        (Set(source), Set())
      } else {
        // walk left and right looking for wall or ledge
        val wallLeft = isWall(source, filled, -1)
        val wallRight = isWall(source, filled, +1)
        val fillPoses = getFillPoses(source, filled, -1)
          .union(getFillPoses(source, filled, 1))
        if (wallLeft && wallRight) {
          (Set(), fillPoses)
        } else {
          (fillPoses, Set())
        }
      }
    }
  }

  def getFillPoses(source: Pos, filled: Set[Pos], offset: Int): Set[Pos] = {
    val posNext = (source._1 + offset, source._2)
    val posBelow = (source._1, source._2 + 1)

    if (filled.contains(posNext) || ! filled.contains(posBelow))
      Set(source)
    else
      getFillPoses(posNext, filled, offset) + source
  }

  def isWall(source: Pos, filled: Set[Pos], offset: Int): Boolean = {
    val posNext = (source._1 + offset, source._2)
    val posBelow = (source._1, source._2 + 1)

    if (filled.contains(posNext)) {
      true
    } else if (! filled.contains(posBelow)) {
      false
    } else {
      isWall(posNext, filled, offset)
    }
  }
}
