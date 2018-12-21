package twelve

import scala.annotation.tailrec

object PartTwo {
  def main(args: Array[String]) {
    println(22 * (BigInt("50000000000") - 94) + 2543)
    val lines = io.Source.stdin.getLines.toList
    val initialGeneration = (parseInitialState(lines.head), 0)
    val patterns = lines.drop(2).filter(line => line.last == '#').map(parsePattern).toSet

    println(trackGenerations(
      initialGeneration,
      patterns,
      0,
      Map.empty,
      Map.empty
    ))
  }

  def getScore(state: (List[Int], Int)): Int = {
    val plantPairs = (state._1 zip (state._2 until state._2 + state._1.size))
    plantPairs.map(pair => pair._1 * pair._2).sum
  }

  @tailrec
  def trackGenerations(
                        generation: (List[Int], Int),
                        patterns: Set[Int],
                        genNum: Int,
                        seenGens: Map[BigInt, Int], // From int repr to gen number
                        genScores: Map[Int, Int] // From gen number to score
                      ): Int = {
    val plantInt = plantPatternToInt(generation._1)
    if (seenGens isDefinedAt plantInt) {
      val lastGenNum = seenGens(plantInt)
      val cycleLength = genNum - lastGenNum
      println(s"genNum: $genNum")
      println(s"lastGenNum: $lastGenNum")
      println(s"cycleLength: $cycleLength")
      println(getScore(generation))
      val finalGenCycleNum = (BigInt("50000000000") - lastGenNum) % cycleLength + lastGenNum
      genScores(finalGenCycleNum.toInt)
    } else {
      val genScore = getScore(generation)
      println(s"genNum: $genNum")
      println(s"genScore: $genScore")
      val newSeenGens = seenGens + (plantInt -> genNum)
      val newGenScores = genScores + (genNum -> genScore)
      trackGenerations(
        makeNextGeneration(generation, patterns),
        patterns,
        genNum + 1,
        newSeenGens,
        newGenScores
      )
    }
  }

  def parsePlant(c: Char): Int = if (c == '#') 1 else 0

  def parseInitialState(line: String): List[Int] = {
    val pattern = "initial state: ([#.]+)".r
    val pattern(stateString) = line
    stateString.map(parsePlant).toList
  }

  def parsePattern(line: String): Int = {
    val pattern = "([#.]+) => #".r
    val pattern(patternString) = line
    plantPatternToInt(patternString.map(parsePlant).toList).toInt
  }

  def plantPatternToInt(plants: List[Int]): BigInt = {
    plants.zipWithIndex
      .map(plantAndPower => plantAndPower._1 * BigInt(2).pow(plantAndPower._2))
      .sum
  }

  def makeNextGeneration(state: (List[Int], Int), patterns: Set[Int]): (List[Int], Int) = {
    val tempState = List(0, 0, 0, 0) ++ state._1 ++ List(0, 0, 0, 0)
    val newPlants = tempState.sliding(5).map(plantPatternToInt).map(patternInt => if (patterns(patternInt.toInt)) 1 else 0).toList
    val droppedNewPlants = newPlants.dropWhile(_ == 0)
    (droppedNewPlants, state._2 - 2 + newPlants.size - droppedNewPlants.size)
  }
}
