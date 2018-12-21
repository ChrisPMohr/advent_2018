package sixteen

object PartTwo {
  def main(args: Array[String]) {
    val lines: List[String] = io.Source.stdin.getLines.toList
    val ops = lines.map(line => parseOpCode(line))

    val registers: Registers = List(0, 0, 0, 0)

    val finalRegisters: Registers = ops.foldLeft(registers)(doOp)
    println(finalRegisters)
    /*
    val opCodesToIndex: List[(Int, List[Int])] = samples
      .map(sample => sample._2.head -> validOpCodes(sample))
    val possibleCodes: Map[Int, List[Int]] = opCodesToIndex
        .groupBy(_._1)
        .map(
          v => (
            v._1 ->
              v._2
                .foldLeft
                  (v._2.head._2)
                  {(s: List[Int], s2: (Int, List[Int])) => s.toSet.intersect(s2._2.toSet).toList}
        )
      )
    for (code <- possibleCodes.toList.sortBy(_._1))
      println(code)
      */
  }

  def doOp(registers: Registers, op: Op): Registers = {
    val opcode = opcodes(op(0))
    opcode(op, registers)
  }

  type Registers = List[Int]
  type Op = List[Int]
  type Sample = (Registers, Op, Registers)

  def parseOpCode(line: String): Op = {
    val opCodePattern = "([0-9 ]+)".r
    val opCodePattern(opCode) = line

    opCode.split(" ").toList.map(_.toInt)
  }

  def parseSample(lines: List[String]): Sample = {
    val beforePattern = raw"Before: \[([0-9 ,]+)\]".r
    val beforePattern(beforeString) = lines(0)

    val opCodePattern = "([0-9 ]+)".r
    val opCodePattern(opCode) = lines(1)

    val afterPattern = raw"After:  \[([0-9 ,]+)\]".r
    val afterPattern(afterString) = lines(2)

    (beforeString.split(", ").toList.map(_.toInt),
      opCode.split(" ").toList.map(_.toInt),
      afterString.split(", ").toList.map(_.toInt))
  }

  def validOpCodes(input: Sample): List[Int] = {
    opcodes
      .zipWithIndex
      .filter(opcodeAndI => opcodeAndI._1(input._2, input._1) == input._3)
      .map(_._2)
  }

  val opcodes = List(
    banr _,
    muli _,
    bori _,
    setr _,
    addi _,
    eqrr _,
    gtri _,
    gtir _,
    borr _,
    eqri _,
    bani _,
    addr _,
    eqir _,
    mulr _,
    seti _,
    gtrr _)

  /*
  val opcodes = List(
    addr _,
    addi _,
    mulr _,
    muli _,
    banr _,
    bani _,
    borr _,
    bori _,
    setr _,
    seti _,
    gtir _,
    gtri _,
    gtrr _,
    eqir _,
    eqri _,
    eqrr _)
 */

  def addr(op: Op, r: Registers): Registers = {
    r.updated(op(3), r(op(1)) + r(op(2)))
  }

  def addi(op: Op, r: Registers): Registers = {
    r.updated(op(3), r(op(1)) + op(2))
  }

  def mulr(op: Op, r: Registers): Registers = {
    r.updated(op(3), r(op(1)) * r(op(2)))
  }

  def muli(op: Op, r: Registers): Registers = {
    r.updated(op(3), r(op(1)) * op(2))
  }

  def banr(op: Op, r: Registers): Registers = {
    r.updated(op(3), r(op(1)) & r(op(2)))
  }

  def bani(op: Op, r: Registers): Registers = {
    r.updated(op(3), r(op(1)) & op(2))
  }

  def borr(op: Op, r: Registers): Registers = {
    r.updated(op(3), r(op(1)) | r(op(2)))
  }

  def bori(op: Op, r: Registers): Registers = {
    r.updated(op(3), r(op(1)) | op(2))
  }

  def setr(op: Op, r: Registers): Registers = {
    r.updated(op(3), r(op(1)))
  }

  def seti(op: Op, r: Registers): Registers = {
    println(op)
    r.updated(op(3), op(1))
  }

  def gtir(op: Op, r: Registers): Registers = {
    r.updated(op(3), if (op(1) > r(op(2))) 1 else 0)
  }

  def gtri(op: Op, r: Registers): Registers = {
    r.updated(op(3), if (r(op(1)) > op(2)) 1 else 0)
  }

  def gtrr(op: Op, r: Registers): Registers = {
    r.updated(op(3), if (r(op(1)) > r(op(2))) 1 else 0)
  }

  def eqir(op: Op, r: Registers): Registers = {
    r.updated(op(3), if (op(1) == r(op(2))) 1 else 0)
  }

  def eqri(op: Op, r: Registers): Registers = {
    r.updated(op(3), if (r(op(1)) == op(2)) 1 else 0)
  }

  def eqrr(op: Op, r: Registers): Registers = {
    r.updated(op(3), if (r(op(1)) == r(op(2))) 1 else 0)
  }

}
