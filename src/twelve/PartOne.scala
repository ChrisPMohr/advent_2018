package twelve

object PartOne {
  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines.toList
    val initialState = (parseInitialState(lines.head), 0)
    val patterns = lines.drop(2).filter(line => line.last == '#').map(parsePattern).toSet
    val lastGeneration = Iterator.iterate(initialState)(state => makeNextGeneration(state, patterns)).drop(20).next
    val lastGenerationStart = lastGeneration._2
    val plantPairs = (lastGeneration._1 zip (lastGenerationStart until lastGenerationStart + lastGeneration._1.size))
    val plantSum = plantPairs.map(pair => pair._1 * pair._2).sum
    println(plantSum)
    //println(lastGeneration._1.map(x => if (x == 1) '#' else '.').mkString(""))
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
    plantPatternToInt(patternString.map(parsePlant).toList)
  }

  def plantPatternToInt(plants: List[Int]): Int = {
    plants.zipWithIndex
      .map(plantAndPower => plantAndPower._1 * math.pow(2, plantAndPower._2))
      .sum.toInt
  }

  def makeNextGeneration(state: (List[Int], Int), patterns: Set[Int]): (List[Int], Int) = {
    val tempState = List(0, 0, 0, 0) ++ state._1 ++ List(0, 0, 0, 0)
    (tempState.sliding(5).map(plantPatternToInt).map(patternInt => if (patterns(patternInt)) 1 else 0).toList, state._2 - 2)
  }
}
