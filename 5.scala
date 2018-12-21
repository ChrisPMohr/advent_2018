import scala.annotation.tailrec

object DayFivePartOne {
    def main(args: Array[String]) {
        var polymer = readLine().toList
        var reactedPolymer = fullyReactPolymer(polymer)
        println(reactedPolymer.size)
    }
    
    def fullyReactPolymer(polymer: List[Char]) = {
        @tailrec
        def _fullyReactPolymer(polymer: List[Char], lastStepPolymer: List[Char]): List[Char] = {
            if (polymer == lastStepPolymer) polymer
            else _fullyReactPolymer(reactPolymer(polymer), polymer)
        }
        _fullyReactPolymer(polymer, Nil)
    }

    def reactPolymer(polymer: List[Char]) = {
        @tailrec
        def _reactPolymer(polymer: List[Char], result: List[Char]): List[Char] = polymer match {
            case Nil => result.reverse
            case c::Nil => _reactPolymer(Nil, c::result)
            case c1::c2::rest => {
                if (doCancel(c1, c2)) _reactPolymer(rest, result) 
                else _reactPolymer(c2::rest, c1::result)
            }
        }
        _reactPolymer(polymer, Nil)
    }
   
    def doCancel(c1: Char, c2: Char): Boolean = {
        doCancelFirstLower(c1, c2) || doCancelFirstLower(c2, c1)
    }
    
    def doCancelFirstLower(c1: Char, c2: Char): Boolean = {
        c1 == c2.toLower && c1.isLower && c2.isUpper
    }
}

object DayFivePartTwo {
    def main(args: Array[String]) {
        var polymer = readLine().toList
        var reactedPolymer = fullyReactPolymer(polymer)
        var allChars = reactedPolymer.map(_.toLower).toSet
        var smallerPolymers = allChars.map(char =>
            char -> fullyReactPolymer(removeChar(reactedPolymer, char)).size
        )
        println(smallerPolymers.minBy(_._2)._2)
    }
    
    def removeChar(polymer: List[Char], removedChar: Char): List[Char] = {
        polymer.filter(c => c.toLower != removedChar)
    }
    
    def fullyReactPolymer(polymer: List[Char]) = {
        @tailrec
        def _fullyReactPolymer(polymer: List[Char], lastStepPolymer: List[Char]): List[Char] = {
            if (polymer == lastStepPolymer) polymer
            else _fullyReactPolymer(reactPolymer(polymer, Nil), polymer)
        }
        _fullyReactPolymer(polymer, Nil)
    }
    
    def reactPolymer(polymer: List[Char]) = {
        @tailrec
        def _reactPolymer(polymer: List[Char], result: List[Char]): List[Char] = polymer match {
            case Nil => result.reverse
            case c::Nil => _reactPolymer(Nil, c::result)
            case c1::c2::rest => {
                if (doCancel(c1, c2)) _reactPolymer(rest, result) 
                else _reactPolymer(c2::rest, c1::result)
            }
        }
        _reactPolymer(polymer, Nil)
    }

    def doCancel(c1: Char, c2: Char): Boolean = {
        doCancelFirstLower(c1, c2) || doCancelFirstLower(c2, c1)
    }
    
    def doCancelFirstLower(c1: Char, c2: Char): Boolean = {
        c1 == c2.toLower && c1.isLower && c2.isUpper
    }
}