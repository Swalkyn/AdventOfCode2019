package day5

import scala.io.Source
import scala.io.StdIn

object IntCode {
    type Memory = List[Int]
    type PC = Int

    sealed trait Instruction {
        val op: Int
        val nParams: Int
        def valByAccess = (mem: Memory, p: Int, a: Int) => if (a == 1) p else mem(p)
        def apply = (mem: Memory, head: Int, ps: List[Int], accs: List[Int]) => (exec(mem, ps, accs), moveHead(mem, head, ps, accs))
        def exec = (mem: Memory, ps: List[Int], accs: List[Int]) => mem
        def moveHead = (mem: Memory, head: Int, ps: List[Int], accs: List[Int]) => head + nParams + 1
    }
    case object Addition extends Instruction { 
        val op = 1
        val nParams = 3
        override def exec = (mem: Memory, ps: List[Int], accs: List[Int]) =>
            mem.updated(ps(2), valByAccess(mem, ps(0), accs(0)) + valByAccess(mem, ps(1), accs(1)))
    }
    case object Multiplication extends Instruction { 
        val op = 2
        val nParams = 3
        override def exec = (mem: Memory, ps: List[Int], accs: List[Int]) =>
            mem.updated(ps(2), valByAccess(mem, ps(0), accs(0)) * valByAccess(mem, ps(1), accs(1)))
    }
    case object Input extends Instruction {
        val op = 3
        val nParams = 1
        override def exec = (mem: Memory, ps: List[Int], accs: List[Int]) =>
            mem.updated(ps.head, StdIn.readInt())
    }
    case object Output extends Instruction {
        val op = 4
        val nParams = 1
        override def exec = (mem: Memory, ps: List[Int], accs: List[Int]) =>
            { println(valByAccess(mem, ps.head, accs.head)); mem }
    }
    case object JumpIfTrue extends Instruction { 
        val op = 5
        val nParams = 2
        override def moveHead = (mem: Memory, head: Int, ps: List[Int], accs: List[Int]) =>
            if (valByAccess(mem, ps(0), accs(0)) != 0) valByAccess(mem, ps(1), accs(1))
            else super.moveHead(mem, head, ps, accs)
    }
    case object JumpIfFalse extends Instruction { 
        val op = 6
        val nParams = 2
        override def moveHead = (mem: Memory, head: Int, ps: List[Int], accs: List[Int]) =>
            if (valByAccess(mem, ps(0), accs(0)) == 0) valByAccess(mem, ps(1), accs(1))
            else super.moveHead(mem, head, ps, accs)
    }
    case object LessThan extends Instruction { 
        val op = 7
        val nParams = 3
        override def exec = (mem: Memory, ps: List[Int], accs: List[Int]) =>
            if (valByAccess(mem, ps(0), accs(0)) < valByAccess(mem, ps(1), accs(1))) mem.updated(valByAccess(mem, ps(2), accs(2)), 1)
            else mem.updated(valByAccess(mem, ps(2), accs(2)), 0)
    }
    case object Equals extends Instruction { 
        val op = 8
        val nParams = 3
        override def exec = (mem: Memory, ps: List[Int], accs: List[Int]) =>
            if (valByAccess(mem, ps(0), accs(0)) == valByAccess(mem, ps(1), accs(1))) mem.updated(valByAccess(mem, ps(2), accs(2)), 1)
            else mem.updated(valByAccess(mem, ps(2), accs(2)), 0)
    }
    case object Halt extends Instruction {
        val op = 99
        val nParams = 0
        override def moveHead = (mem: Memory, head: Int, ps: List[Int], accs: List[Int]) => -1
    }
}

object Main extends App {
    import IntCode._

    val input = Source.fromFile("src/main/resources/day5/input.txt").mkString.stripLineEnd.split(',').toList

    val supportedInstr: List[Instruction] = List(Addition, Multiplication, Input, Output, JumpIfTrue, JumpIfFalse, LessThan, Equals, Halt)
    val opcodeMap = supportedInstr.map((i: Instruction) => (i.op, i)).toMap//.withDefault(throw new IllegalArgumentException("Unknown opcode"))
    
    def getDigitAt(n: Int, pos: Int): Int = 
        (n / Math.pow(10, pos).toInt) % 10

    def cycle(mem: Memory, head: Int): (List[Int], Int) = {
        val memoryAtHead = mem.drop(head)
        val opcode = memoryAtHead.head
        val instr = opcodeMap(opcode % 100)
        val params = memoryAtHead.tail.take(instr.nParams)
        val accesses = (0 until instr.nParams).map(n => getDigitAt(opcode, n + 2)).toList

        //println(f"Head at addr ${head}")
        println(f"Instruction: ${instr}, params: ${params}, accesses: ${accesses}")

        instr.apply(mem, head, params, accesses)
    }

    def run: ((List[Int], Int)) => Unit = { 
        case(mem, head) => if (head >= 0) {
            val (newMem, newHead) = cycle(mem, head)
            run(newMem, newHead)
        }
    }

    val memory: Memory = input.map(_.toInt).toList
    val testMemory: Memory = List(
        3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
    )

    // Part 1
    run((testMemory, 0))

    // Part 2
}
