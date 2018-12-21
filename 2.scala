object DayTwoPartOne {
    def main(args: Array[String]) {
        var lines = io.Source.stdin.getLines.toList
        println(lines.filter(hasDuplicates).size *
                lines.filter(hasTriplicates).size)
    }

    def countCharOccurences(ln: String): Map[Char, Int] = ln.groupBy(identity).mapValues(_.size)
    
    def invertMap(m: Map[Char, Int]): Map[Int, Iterable[Char]] = m.groupBy(_._2).mapValues(_.keys)
    
    def hasDuplicates(ln: String): Boolean = invertMap(countCharOccurences(ln)).contains(2)
    
    def hasTriplicates(ln: String): Boolean = invertMap(countCharOccurences(ln)).contains(3)
}

object DayTwoPartTwo {
    def main(args: Array[String]) {
        var lines = io.Source.stdin.getLines.toList
        var firstOffByOne = getOffByOne(lines).take(1).toList.headOption.get
        println(Function.tupled(getSameChars _)(firstOffByOne))
    }

    def getSameChars(ln1: String, ln2: String) = {
        (ln1 zip ln2).filter((chars) => chars._1 == chars._2).map(_._1).mkString("")
    }

    def isOffByOne(ln1: String, ln2: String) = {
        ln1.size - getSameChars(ln1, ln2).size == 1
    }

    def getOffByOne(lines: List[String]) = {
        lines
            .combinations(2)
            .map(l => (l(0), l(1)))
            .filter(Function.tupled(isOffByOne))
    }    
}

object DayTwoPartOneOld {
    def main(args: Array[String]) {
        var lines = io.Source.stdin.getLines.toList
        println(lines.filter(hasExactDuplicates).size *
                lines.filter(hasExactTriplicates).size)
    }
   
   def hasExactDuplicates(ln: String): Boolean = {
        def helper(args: (Set[Char], Set[Char], Set[Char]), cur: Char) = {
            var seenOnce = args._1
            var seenTwice = args._2
            var seenThree = args._3
            if (seenThree(cur)) (seenOnce, seenTwice, seenThree)
            else {
                if (seenOnce(cur)) {
                    if (seenTwice(cur)) (seenOnce, seenTwice, seenThree + cur)
                    else (seenOnce, seenTwice + cur, seenThree)
                } else (seenOnce + cur, seenTwice, seenThree)
            }
        }
        val ret = ln.foldLeft((Set.empty[Char], Set.empty[Char], Set.empty[Char]))(helper)
        val numDuplicates = ret._2.size - ret._3.size
        numDuplicates > 0
    }
    
    def hasExactTriplicates(ln: String): Boolean = {
        def helper(args: (Set[Char], Set[Char], Set[Char], Set[Char]), cur: Char) = {
            var seenOnce = args._1
            var seenTwice = args._2
            var seenThree = args._3
            var seenFour = args._4
            if (seenFour(cur)) (seenOnce, seenTwice, seenThree, seenFour)
            else {
                if (seenOnce(cur)) {
                    if (seenTwice(cur)) {
                        if (seenThree(cur)) (seenOnce, seenTwice, seenThree, seenFour + cur)
                        else (seenOnce, seenTwice, seenThree + cur, seenFour)
                    } else (seenOnce, seenTwice + cur, seenThree, seenFour)
                } else (seenOnce + cur, seenTwice, seenThree, seenFour)
            }
        }
        val ret = ln.foldLeft((Set.empty[Char], Set.empty[Char], Set.empty[Char], Set.empty[Char]))(helper)
        val numTriplicates = ret._3.size - ret._4.size
        numTriplicates > 0
    }
}
