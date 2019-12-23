package intcode

import scala.io.Source
import scala.io.StdIn
import scala.annotation.tailrec

/*
Possible improvements
 - use a map for the s.memory, for faster updates
 - use a lazylist with unfold instead of cycle
 - redesign IO
*/

object IntCode {
    type Memory = Map[Long, Long]
    type PC = Long

    case class Parameter(value: Long, access: Int)
    case class State(mem: Memory, head: PC, rel: PC, in: LazyList[Long], out: Option[Long])

    sealed trait Instruction {
        val op: Int
        val nParams: Int
        def readAccess = (s: State, p: Parameter) => p.access match {
            case 0 => s.mem(p.value)
            case 1 => p.value
            case 2 => s.mem(s.rel + p.value)
            case _ => throw new IllegalArgumentException
        }

        def writeAccess = (s: State, p: Parameter) => p.access match {
            case 0 => p.value
            case 1 => p.value
            case 2 => s.rel + p.value
            case _ => throw new IllegalArgumentException
        }

        def apply(s: State, p: List[Parameter]): Option[State] =
            halted(State(exec(s, p), moveHead(s, p), moveRBase(s, p), input(s, p), output(s, p)))
        
        def halted(s: State): Option[State] = Some(s)
        def exec(s: State, p: List[Parameter]): Memory = s.mem
        def moveHead(s: State, p: List[Parameter]): PC = s.head + nParams + 1
        def moveRBase(s: State, p: List[Parameter]): PC = s.rel
        def input(s: State, p: List[Parameter]): LazyList[Long] = s.in
        def output(s: State, p: List[Parameter]): Option[Long] = None
    }
    case object Addition extends Instruction { 
        val op = 1
        val nParams = 3
        override def exec(s: State, p: List[Parameter]) =
            s.mem.updated(writeAccess(s, p(2)), readAccess(s, p(0)) + readAccess(s, p(1)))
    }
    case object Multiplication extends Instruction { 
        val op = 2
        val nParams = 3
        override def exec(s: State, p: List[Parameter]) =
            s.mem.updated(writeAccess(s, p(2)), readAccess(s, p(0)) * readAccess(s, p(1)))
    }
    case object Input extends Instruction {
        val op = 3
        val nParams = 1
        override def exec(s: State, p: List[Parameter]) =
            s.mem.updated(writeAccess(s, p.head), s.in.applyOrElse(0, (i: Int) => { println("Input needed:"); StdIn.readInt() }))
        override def input(s: State, p: List[Parameter]): LazyList[Long] =
            if (s.in.isEmpty) s.in else s.in.tail
    }
    case object Output extends Instruction {
        val op = 4
        val nParams = 1
        override def output(s: State, p: List[Parameter]) =
            Some(readAccess(s, p.head))
    }
    case object JumpIfTrue extends Instruction { 
        val op = 5
        val nParams = 2
        override def moveHead(s: State, p: List[Parameter]) = 
            if (readAccess(s, p(0)) != 0) readAccess(s, p(1))
            else super.moveHead(s, p)
    }
    case object JumpIfFalse extends Instruction { 
        val op = 6
        val nParams = 2
        override def moveHead(s: State, p: List[Parameter]) = 
            if (readAccess(s, p(0)) == 0) readAccess(s, p(1))
            else super.moveHead(s, p)
    }
    case object LessThan extends Instruction { 
        val op = 7
        val nParams = 3
        override def exec(s: State, p: List[Parameter]) =
            if (readAccess(s, p(0)) < readAccess(s, p(1))) s.mem.updated(writeAccess(s, p(2)), 1)
            else s.mem.updated(writeAccess(s, p(2)), 0)
    }
    case object Equals extends Instruction { 
        val op = 8
        val nParams = 3
        override def exec(s: State, p: List[Parameter]) =
            if (readAccess(s, p(0)) == readAccess(s, p(1))) s.mem.updated(writeAccess(s, p(2)), 1)
            else s.mem.updated(writeAccess(s, p(2)), 0)
    }
    case object RBase extends Instruction {
        val op = 9
        val nParams = 1
        override def moveRBase(s: State, p: List[Parameter]) = 
            s.rel + readAccess(s, p.head)
    }
    case object Halt extends Instruction {
        val op = 99
        val nParams = 0
        override def halted(s: State) = None
    }
    
    def getDigitAt(n: Long, pos: Int): Int = 
        (n / Math.pow(10, pos).toInt).toInt % 10
    
    def exec(s: State): Option[State] = {
        val opcode = s.mem(s.head)
        val instr = opcodeMap(opcode.toInt % 100)
        val values = (1 to instr.nParams).map(n => s.mem(s.head + n))
        val accesses = (0 until instr.nParams).map(n => getDigitAt(opcode, n + 2)).toList
        val params = (values zip accesses).map(p => Parameter(p._1, p._2)).toList
        
        // println(f"Instruction: ${instr}, params: ${params}")
        
        instr(s, params)
    }

    def run(initialState: State): LazyList[Long] = {
        def cycle: State => Option[(Option[Long], State)] = 
            exec(_).flatMap(s => Some((s.out, s)))

        LazyList.unfold(initialState)(cycle).flatten
    }

    def runWith(mem: Map[Long, Long], in: LazyList[Long] = LazyList()): LazyList[Long] =
        run(State(mem, 0, 0, in, None))

    def step(s: State): State = {
        def cycle: State => Option[(Option[State], State)] =
            exec(_).flatMap(s => Some((s.out.flatMap(out => Some(s)), s)))

        LazyList.unfold(s)(cycle).flatten.head
    }
    
    def parseString(input: String): Map[Long, Long] =
        input
            .stripLineEnd.split(',')
            .toList.map(_.toLong)
            .zipWithIndex
            .map(x => (x._2.toLong, x._1))
            .toMap.withDefaultValue(0)
        
    val supportedInstr: List[Instruction] = List(Addition, Multiplication, Input, Output, JumpIfTrue, JumpIfFalse, LessThan, Equals, RBase, Halt)
    val opcodeMap = supportedInstr.map((i: Instruction) => (i.op, i)).toMap
}

// Test the computer here
object Main extends App {
    import IntCode._
    
    val input = Source.fromFile("src/main/resources/day5/input.txt").mkString
    val memory = IntCode.parseString(input)
    val testmemory = List(
        3,9,8,9,10,9,4,9,99,-1,8
        )
    
    lazy val result = IntCode.runWith(memory, in=LazyList(5))
    println(result.toList)
}