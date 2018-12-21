object HelloWorld {
    def main(args: Array[String]) {
        val lines = io.Source.stdin.getLines.toList
        val reqs = lines.map(parseReq)
        val nexts = reqs.groupBy(_._1).mapValues(l => l.map(_._2).toSet)
        val prereqs = reqs.groupBy(_._2).mapValues(l => l.map(_._1).toSet)
        val starts = (reqs.map(_._1).toSet diff prereqs.keys.toSet)
        val start = starts.toList.sortBy(identity).head
        println(walkGraph(start, nexts, starts, prereqs, Set()).mkString(""))

    }
    
    def parseReq(line: String): (String, String) = {
        val pattern = "Step (.) must be finished before step (.) can begin.".r
        val pattern(x, y) = line
        (x, y)
    }
    
    def walkGraph(
            current: String,
            nexts: Map[String, Set[String]],
            free: Set[String],
            prereqs: Map[String, Set[String]],
            completed: Set[String]): List[String] = {
        val newCompleted = completed + current
        val possibleNexts = newCompleted
                .flatMap(step => nexts.getOrElse(step, Set()))
                .union(free)
                .filter(step => !newCompleted(step))
        //println(possibleNexts)
        val validNexts = possibleNexts
            .filter(step => prereqs.getOrElse(step, Set()) subsetOf newCompleted)
        //println(validNexts)
        val allChars = nexts.keys.toSet.union(prereqs.keys.toSet)
        if (newCompleted.size != allChars.size) {
            if (validNexts.size > 0) {
                val next = validNexts.minBy(identity)
                current::walkGraph(next, nexts, free, prereqs, newCompleted)
            } else current::Nil
        } else current::Nil
    }
}

object HelloWorld {
    def main(args: Array[String]) {
        val lines = io.Source.stdin.getLines.toList
        val reqs = lines.map(parseReq)
        val nexts = reqs.groupBy(_._1).mapValues(l => l.map(_._2).toSet)
        val prereqs = reqs.groupBy(_._2).mapValues(l => l.map(_._1).toSet)
        val starts = (reqs.map(_._1).toSet diff prereqs.keys.toSet)
        val numWorkers = 5
        val inProgress = getFirstInProgress(starts, numWorkers)
        println(inProgress)
        println(walkGraph(inProgress, numWorkers, nexts, starts, prereqs, Set()))
    }
    
    def parseReq(line: String): (String, String) = {
        val pattern = "Step (.) must be finished before step (.) can begin.".r
        val pattern(x, y) = line
        (x, y)
    }
    
    def workTime(c: String) = {
        c(0).toInt - 64 + 60
    }
    
    
    def getFirstInProgress(starts: Set[String], numWorkers: Int) = {
        val sortedStart = starts.toList.sortBy(identity)
        val firstWorked = sortedStart.slice(0, numWorkers)
        firstWorked.map(c => c -> workTime(c))
    }
    
    def walkGraph(
            inProgress: List[(String, Int)],
            numWorkers: Int,
            nexts: Map[String, Set[String]],
            free: Set[String],
            prereqs: Map[String, Set[String]],
            completed: Set[String]): Int = {
        //println(inProgress)
        val nextSecondInProgress = inProgress.map(v => (v._1, v._2 - 1))
        val justFinished = nextSecondInProgress.filter(v => v._2 == 0).map(_._1)
        if (justFinished.size > 0)
        {
            val newCompleted = completed.union(justFinished.toSet)
            val filteredInProgress = nextSecondInProgress.filter(v => v._2 != 0)
            val completedOrInProgress = newCompleted.union(filteredInProgress.map(_._1).toSet)
            val possibleNexts = newCompleted
                    .flatMap(step => nexts.getOrElse(step, Set()))
                    .union(free)
                    .filter(step => !completedOrInProgress(step))
            //println(possibleNexts)
            val validNexts = possibleNexts
                .filter(step => prereqs.getOrElse(step, Set()) subsetOf newCompleted)
            //println(validNexts)
            val allChars = nexts.keys.toSet.union(prereqs.keys.toSet)
            if (newCompleted.size != allChars.size) {
                if (validNexts.size > 0) {
                    val newInProgress = filteredInProgress:::getFirstInProgress(validNexts, numWorkers - filteredInProgress.size)
                    1 + walkGraph(newInProgress, numWorkers, nexts, free, prereqs, newCompleted)
                } else 1 + walkGraph(filteredInProgress, numWorkers, nexts, free, prereqs, newCompleted)
            } else 1
        } else {
            1 + walkGraph(nextSecondInProgress, numWorkers, nexts, free, prereqs, completed)
        }
    }
}