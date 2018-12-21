object DayFourPartOne {
    def main(args: Array[String]) {
        var lines = io.Source.stdin.getLines.toList
        var logs = lines.map(parseLine).sortBy(t => (t._1, t._2, t._3))
        var logsByGuard: Map[Int, List[LogLine]] = addGuardToLogs(logs, -1).groupBy(_._1).mapValues(_.map(_._2))
        var asleepIntervals: Map[Int, List[(Int, Int)]] = logsByGuard.mapValues(
            logs => logs.map(_._3).grouped(2).map(
                l => (l(0), l(1))
            ).toList)
        var asleepMinutes: Map[Int, List[Int]] = asleepIntervals.mapValues(
            intervals => intervals.flatMap(
                interval => interval._1 until interval._2))
        var mostSleepy: Int = asleepMinutes.maxBy(_._2.size)._1
        var sleepiestMinute: Int = asleepMinutes(mostSleepy).groupBy(identity).maxBy(_._2.size)._1
        println(mostSleepy * sleepiestMinute)
    }

    type LogLine = (String, Int, Int, String)
    
    def parseLine(line: String): LogLine = {
        val pattern = "\\[([0-9\\-]+) ([0-9]+):([0-9]+)\\] (.*)".r
        val pattern(date, hour, minute, message) = line
        (date, hour.toInt, minute.toInt, message)
    }
    
    val guardPattern = "Guard #([0-9]+) .*".r

    def addGuardToLogs(logs: List[LogLine], lastGuard: Int): List[(Int, LogLine)] = logs match {
        case Nil => Nil
        case log::tail => {
            log._4 match {
                case guardPattern(newGuardStr) => addGuardToLogs(tail, newGuardStr.toInt)
                case _ => (lastGuard, log)::addGuardToLogs(tail, lastGuard)
            }
        }
    }
}

object DayFourPartTwo {
    def main(args: Array[String]) {
        var lines = io.Source.stdin.getLines.toList
        var logs = lines.map(parseLine).sortBy(t => (t._1, t._2, t._3))
        var logsByGuard: Map[Int, List[LogLine]] = addGuardToLogs(logs, -1).groupBy(_._1).mapValues(_.map(_._2))
        var asleepIntervals: Map[Int, List[(Int, Int)]] = logsByGuard.mapValues(
            logs => logs.map(_._3).grouped(2).map(
                l => (l(0), l(1))
            ).toList)
        var asleepMinutes: Map[Int, List[Int]] = asleepIntervals.mapValues(
            intervals => intervals.flatMap(
                interval => interval._1 until interval._2))
        var guardsAndMinutes: List[(Int, Int)] = asleepMinutes.map(kv => kv._2 map ((kv._1, _))).flatten
        var sleepiestGuardAndMinute: (Int, Int) = guardsAndMinutes.groupBy(identity).maxBy(_._2.size)._1
        println(sleepiestGuardAndMinute._1 * sleepiestGuardAndMinute._2)
    }
    
    type LogLine = (String, Int, Int, String)
    
    def parseLine(line: String): LogLine = {
        val pattern = "\\[([0-9\\-]+) ([0-9]+):([0-9]+)\\] (.*)".r
        val pattern(date, hour, minute, message) = line
        (date, hour.toInt, minute.toInt, message)
    }
    
    val guardPattern = "Guard #([0-9]+) .*".r

    def addGuardToLogs(logs: List[LogLine], lastGuard: Int): List[(Int, LogLine)] = logs match {
        case Nil => Nil
        case log::tail => {
            log._4 match {
                case guardPattern(newGuardStr) => addGuardToLogs(tail, newGuardStr.toInt)
                case _ => (lastGuard, log)::addGuardToLogs(tail, lastGuard)
            }
        }
    }
}