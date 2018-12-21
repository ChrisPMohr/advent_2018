object DayThreePartOne {
    def main(args: Array[String]) {
        var lines = io.Source.stdin.getLines.toList
        var claimed = lines.flatMap(line => claimToList _ tupled strToClaim(line))
        var occurences = countOccurences(claimed)
        println(occurences.filter(_._2 > 1).keys.size)
    }
    
    def strToClaim(line: String) = {
        val pattern = "#[0-9]+ @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
        val pattern(x, y, width, height) = line
        (x.toInt, y.toInt, width.toInt, height.toInt)
    }
    
    implicit class Crossable[X](xs: Traversable[X]) {
        def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
    }

    def claimToList(x: Int, y: Int, width: Int, height: Int) = {
        ((0 until width) cross (0 until height)).map(pair => (x + pair._1, y + pair._2))
    }

    def countOccurences[T](ln: List[T]): Map[T, Int] = ln.groupBy(identity).mapValues(_.size)
}

object DayThreePartTwo {
    def main(args: Array[String]) {
        var lines = io.Source.stdin.getLines.toList
        var claimed = lines.flatMap(line => claimToList _ tupled strToClaim(line))
        var overlaps = getOverlaps(claimed).values.flatten.toSet
        var allClaims = claimed.map(_._1).toSet
        println(allClaims diff overlaps)
    }
    
    def strToClaim(line: String) = {
        val pattern = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
        val pattern(claim, x, y, width, height) = line
        (claim.toInt, x.toInt, y.toInt, width.toInt, height.toInt)
    }
    
    implicit class Crossable[X](xs: Traversable[X]) {
        def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
    }

    def claimToList(claim: Int, x: Int, y: Int, width: Int, height: Int) = {
        ((0 until width) cross (0 until height))
            .map(pair => 
                 (claim, (x + pair._1, y + pair._2)))
    }

    def getOverlaps(claim: List[(Int, Any)]) = {
        claim
            .groupBy(_._2)
            .mapValues(l => l.map(_._1))
            .filter(_._2.size > 1)
    }
}