object DayOnePartOne {
    def main(args: Array[String]) {
        println(
            io.Source.stdin.getLines.map(toNumber).sum
        )
    }
   
   def toNumber(ln: String): Int = ln(0) match {
       case '+' => ln.substring(1).toInt
       case '-' => ln.toInt
    }
}

object DayOnePartTwo {
    def main(args: Array[String]) {
        var numbers = io.Source.stdin.getLines.map(toNumber).toList
        println(getFirstRepeatedCumSum(
            Stream.continually(numbers.toStream).flatten))
    }
        
    def toNumber(ln: String): Int = ln(0) match {
       case '+' => ln.substring(1).toInt
       case '-' => ln.toInt
    }
    
    def getFirstRepeatedCumSum(list: Iterable[Int]): Option[Int] = {
        def helper(it: Iterator[Int],
                   sum: Int,
                   seenSums: Set[Int]): Option[Int] = {
            if (it.hasNext) {
                val x = it.next
                val new_sum = sum + x
                if (seenSums(new_sum)) Some(new_sum)
                else helper(it, new_sum, seenSums + new_sum)
            } else None
        }
        helper(list.iterator, 0, Set.empty[Int])
    }

}