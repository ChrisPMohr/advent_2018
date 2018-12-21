package fifteen

object PartTwo {
  val size = 32
  val health = 200

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines.toList
    val linesWithIndex: List[(Char, Pos)] = lines.zipWithIndex.flatMap(
      el => el._1.zipWithIndex.map(el2 => (el2._1, (el2._2, el._2))).toList
    )
    val walls: Set[Pos] = linesWithIndex.filter(_._1 == '#').map(_._2).toSet
    val initialUnits: Vector[Unit] = linesWithIndex
      .filter(cAndPos => Set('E', 'G').contains(cAndPos._1))
      .map(cAndPos => (cAndPos._2, cAndPos._1, health))
      .toVector

    val finalState = tryAttacks(initialUnits, walls, 4)
    println(finalState._2)
    printState(finalState._1, walls)
    println((finalState._2 - 1) * finalState._1.map(_._3).sum)
  }

  type Pos = (Int, Int)
  type Team = Char
  type Hp = Int
  type Unit = (Pos, Team, Hp)

  def printState(units: Vector[Unit], walls: Set[Pos]): scala.Unit = {
    val unitsByPos: Map[Pos, Unit] = units.map(u => u._1 -> u).toMap
    for (y <- 0 until size) {
      for (x <- 0 until size) {
        val pos = (x, y)
        if (walls.contains(pos))
          print('#')
        else if (unitsByPos.contains(pos))
          print(unitsByPos(pos)._2)
        else
          print('.')
      }
      println("")
    }
    println("")
    println(units.map(_._3).mkString(" "))
    println("")
  }

  def tryAttacks(initialUnits: Vector[Unit], walls: Set[Pos], attack: Int): (Vector[Unit], Int) = {
    println(s"Trying attack $attack")
    def runUntilEnd(units: Vector[Unit], walls: Set[Pos], round: Int): (Vector[Unit], Int) = {
      val elfCount = numElves(units)
      val nextUnits = doTick(units, walls)
      if (isDone(nextUnits) || elfCount != numElves(nextUnits)) {
        (nextUnits, round)
      } else {
        runUntilEnd(nextUnits, walls, round + 1)
      }
    }

    def numElves(units: Vector[Unit]): Int = {
      units.count(_._2 == 'E')
    }

    def isDone(units: Vector[Unit]): Boolean = {
      units.map(_._2).toSet.size < 2
    }

    def doTick(units: Vector[Unit], walls: Set[Pos]): Vector[Unit] = {
      val sortedUnitIndexes = units
        .zipWithIndex
        .sortBy(unit => posSortOrder(unit._1._1))
        .map(_._2)

      val optionalUnits: Vector[Option[Unit]] = units.map(u => Option(u))

      val part: (Vector[Option[Unit]], Int) => Vector[Option[Unit]] = doUnitTurn(
        _: Vector[Option[Unit]], _: Int, walls)

      sortedUnitIndexes.foldLeft(optionalUnits)(part).flatten
    }

    def doUnitTurn(units: Vector[Option[Unit]], i: Int, walls: Set[Pos]): Vector[Option[Unit]] = {
      if (i < units.size) {
        val updatedUnits = doUnitMove(i, units, walls)
        doUnitAttack(i, updatedUnits)
      } else
        units
    }

    def doUnitAttack(i: Int, units: Vector[Option[Unit]]): Vector[Option[Unit]] = {
      if (units(i).isEmpty)
        units
      else {
        val unit: Unit = units(i).get
        val unitPos: Pos = unit._1
        val enemyUnitsByPos: Map[Pos, Unit] = units.flatten.filter(u => u._2 != unit._2).map(u => u._1 -> u).toMap

        val inRangeSpaces: Set[Pos] = getInRangeSpaces(unitPos).toSet
        val targetableSpaces: Set[Pos] = inRangeSpaces.intersect(enemyUnitsByPos.keySet)
        if (targetableSpaces.isEmpty) {
          units
        } else {
          val targetableEnemyUnitsByPos: Set[Unit] = targetableSpaces.map(pos => enemyUnitsByPos(pos))
          val target = targetableEnemyUnitsByPos.minBy(unit => (unit._3, posSortOrder(unit._1)))
          val unitAttack = if (unit._2 == 'E') attack else 3
          val updatedTarget = (target._1, target._2, math.max(target._3 - unitAttack, 0))
          val targetIndex = units.indexOf(Option(target))
          if (updatedTarget._3 == 0)
            units.updated(targetIndex, None)
          else
            units.updated(targetIndex, Option(updatedTarget))
        }
      }
    }

    def doUnitMove(i: Int, units: Vector[Option[Unit]], walls: Set[Pos]): Vector[Option[Unit]] = {
      if (units(i).isEmpty)
        units
      else {
        val unit: Unit = units(i).get
        val unitPos: Pos = unit._1
        val unitPoses: Set[Pos] = units.flatten.map(_._1).toSet
        val enemyUnits: Vector[Unit] = units.flatten.filter(u => u._2 != unit._2)

        val spacesInRangeOfTargets: Vector[Pos] = enemyUnits
          .map(_._1)
          .flatMap(pos => getInRangeSpaces(pos))

        if (spacesInRangeOfTargets.contains(unitPos)) {
          units
        } else {
          val getDistances = dijkstra(makeGraph(unitPoses.union(walls), _: Pos)) _
          // println(s"unit $unitPos")
          val target: Option[Pos] = getClosestOfOptions(unitPos, spacesInRangeOfTargets, getDistances)
          // println(s"target $target")

          if (target.isDefined) {
            val spacesInRangeOfPos: List[Pos] = getInRangeSpaces(unitPos)
            val stepPos: Option[Pos] = getClosestOfOptions(target.get, spacesInRangeOfPos, getDistances)

            units.updated(i, Option((stepPos.get, unit._2, unit._3)))
          } else
            units
        }
      }
    }

    val endState = runUntilEnd(initialUnits, walls, 1)
    if (isDone(endState._1)) {
      endState
    } else {
      tryAttacks(initialUnits, walls, attack + 1)
    }
  }

  def posSortOrder(pos: Pos): (Int, Int) = {
    (pos._2, pos._1)
  }

  def getClosestOfOptions(startPos: Pos,
                          endPoses: Traversable[Pos],
                          getDistances: Pos => Map[Pos, Int]): Option[Pos] ={
    val allDistances: Map[Pos, Int] = getDistances(startPos)
    val distancesToTargets: Traversable[(Pos, Int)] = endPoses
      .toSet
      .intersect(allDistances.keySet)
      .map(p => (p, allDistances(p)))
    if (distancesToTargets.isEmpty)
      None
    else {
      val closestDistance = distancesToTargets.minBy(_._2)._2
      Option(distancesToTargets
        .filter(posAndDistance => posAndDistance._2 == closestDistance)
        .minBy(posAndDistance => posSortOrder(posAndDistance._1))
        ._1)
    }
  }

  def getInRangeSpaces(pos: Pos): List[Pos] = {
    List(
      (pos._1 + 1, pos._2),
      (pos._1 - 1, pos._2),
      (pos._1, pos._2 + 1),
      (pos._1, pos._2 - 1))
  }

  def makeGraph(filledPoses: Set[Pos], pos: Pos) : Map[N, Int]= {
    val inRangeSpaces = getInRangeSpaces(pos).toSet
    inRangeSpaces
      .diff(filledPoses)
      .filter(p => p._1 >= 0 && p._1 < size && p._2 >= 0 && p._2 < size)
      .map(_ -> 1)
      .toMap
  }

  type N = Pos
  type Graph[N] = N => Map[N, Int]
  def dijkstra[N](g: Graph[N])(source: N): Map[N, Int] = {
    def go(active: Set[N], res: Map[N, Int], pred: Map[N, N]):
    (Map[N, Int], Map[N, N]) =
      if (active.isEmpty) (res, pred)
      else {
        val node = active.minBy(res)
        val cost = res(node)
        val neighbours = for {
          (n, c) <- g(node) if
          cost + c < res.getOrElse(n, Int.MaxValue)
        } yield n -> (cost + c)
        val active1 = active - node ++ neighbours.keys
        val preds = neighbours mapValues (_ => node)
        go(active1, res ++ neighbours, pred ++ preds)
      }

    go(Set(source), Map(source -> 0), Map.empty)._1
  }
}
