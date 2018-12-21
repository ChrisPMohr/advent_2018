package thirteen

object PartTwo {
  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines.toList
    val linesWithIndex: List[List[(Char, (Int, Int))]] = lines.zipWithIndex.map(
      el => el._1.zipWithIndex.map(el2 => (el2._1, (el2._2, el._2))).toList
    )
    val initial: List[(TrackSpace, (Int, Int))] = linesWithIndex.flatMap(parseTrackRow)
    val carts: Carts = initial.filter(_._1._2.isDefined).map(el => el._2 -> el._1._2.get).toMap
    val track: Track = initial.map(el => el._2 -> el._1._1).toMap
    println(getLastCartPos(track, carts))
  }

  type Carts = Map[(Int, Int), Cart]
  type Direction = Int // 0: up, 1: right, 2: down, 3: left
  type Cart = (Direction, Int)
  type ValidDirections = Set[Direction]
  type TrackSpace = (ValidDirections, Option[Cart])
  type Track = Map[(Int, Int), ValidDirections]

  def parseTrackSpace(char: Char, prev: Char): TrackSpace = char match {
    case '-' => (Set(1, 3), None)
    case '|' => (Set(0, 2), None)
    case '/' =>
      if (prev == '-' || prev == '+' || prev == '>' || prev == '<')
        (Set(0, 3), None)
      else
        (Set(1, 2), None)
    case '\\' =>
      if (prev == '-' || prev == '+' || prev == '>' || prev == '<')
        (Set(2, 3), None)
      else
        (Set(0, 1), None)
    case '+' => (Set(0, 1, 2, 3), None)
    case '^' => (Set(0, 2), Some((0, 0)))
    case '>' => (Set(1, 3), Some((1, 0)))
    case 'v' => (Set(0, 2), Some((2, 0)))
    case '<' => (Set(1, 3), Some((3, 0)))
    case _ => (Set(), None)
  }

  def parseTrackRow(line: List[(Char, (Int, Int))]): List[(TrackSpace, (Int, Int))] = {
    (List((' ', (0, 0))) ++ line).sliding(2).map(els =>
      (parseTrackSpace(els(1)._1, els(0)._1), els(1)._2)).toList
  }

  def doTick(track: Track, carts: Carts): (Track, Carts) = {
    val sortedCarts: List[((Int, Int), Cart)] = carts.toList.sortBy(cart => (cart._1._2, cart._1._1))
    val newCarts: List[((Int, Int), Cart)] = moveAllCarts(track, carts, Nil)
    (track, newCarts.toMap)
  }

  def moveAllCarts(track: Track, carts: Carts, movedCarts: List[((Int, Int), Cart)]): List[((Int, Int), Cart)] = {
    if (carts.isEmpty) {
      movedCarts
    } else {
      val cart = carts.head
      val newCart = getNextCartPosition(cart, track)
      val currentPoses = movedCarts.map(_._1).toSet ++ carts.keySet
      if (currentPoses.contains(newCart._1)) {
        val filteredMoved = movedCarts.filter(el => el._1 != newCart._1)
        val filteredCarts = carts.tail.filter(el => el._1 != newCart._1)
        moveAllCarts(track, filteredCarts, filteredMoved)
      } else {
        moveAllCarts(track, carts.tail, newCart::movedCarts)
      }
    }
  }

  def getNextCartPosition(cartAndPos: ((Int, Int), Cart), track: Track): ((Int, Int), Cart) = {
    val pos = cartAndPos._1
    val cart = cartAndPos._2
    val dir = cart._1
    val nextPos = dir match {
      case 0 => (pos._1, pos._2 - 1)
      case 1 => (pos._1 + 1, pos._2)
      case 2 => (pos._1, pos._2 + 1)
      case 3 => (pos._1 - 1, pos._2)
    }

    val nextTrack: ValidDirections = track(nextPos)
    if (nextTrack.size == 4) {
      val turnState = cart._2
      val nextTurnState = (turnState + 1) % 3
      val nextDir = (dir + (turnState - 1) + 4) % 4
      (nextPos, (nextDir, nextTurnState))
    } else {
      val nextDir = if (nextTrack.contains(dir)) dir else {
        (nextTrack - ((dir + 2) % 4)).head
      }
      (nextPos, (nextDir, cart._2))
    }
  }

  def getLastCartPos(track: Track, carts: Carts): (Int, Int) = {
    val newCarts = doTick(track, carts)._2
    if (newCarts.size == 1) {
      println(carts)
      println(newCarts)
      newCarts.toList.head._1
    } else getLastCartPos(track, newCarts)
  }
}
