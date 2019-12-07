package intcode

import scala.io.Source
import scala.io.StdIn

object IntCode {
    type Memory = List[Int]
    type PC = Int
    type IO = List[Int]

    case class Parameter(value: Int, access: Int)
    case class State(mem: Memory, head: PC, in: IO, out: IO)

    sealed trait Instruction {
        val op: Int
        val nParams: Int
        def valByAccess = (mem: Memory, p: Parameter) => if (p.access == 1) p.value else mem(p.value)

        def apply(p: List[Parameter])(s: State): State = s match {
            case State(mem, head, in, out) => 
                State(exec(p)(mem, in), moveHead(p)(mem, head), input(p)(in), output(p)(mem, out))
        }
        
        def exec(p: List[Parameter])(mem: Memory, in: IO): Memory = mem
        def moveHead(p: List[Parameter])(mem: Memory, head: Int): PC = head + nParams + 1
        def input(p: List[Parameter])(in: IO): IO = in
        def output(p: List[Parameter])(mem: Memory, out: IO): IO = out
    }
    case object Addition extends Instruction { 
        val op = 1
        val nParams = 3
        override def exec(p: List[Parameter])(mem: Memory, in: IO) =
            mem.updated(p(2).value, valByAccess(mem, p(0)) + valByAccess(mem, p(1)))
    }
    case object Multiplication extends Instruction { 
        val op = 2
        val nParams = 3
        override def exec(p: List[Parameter])(mem: Memory, in: IO) =
            mem.updated(p(2).value, valByAccess(mem, p(0)) * valByAccess(mem, p(1)))
    }
    case object Input extends Instruction {
        val op = 3
        val nParams = 1
        override def exec(p: List[Parameter])(mem: Memory, in: IO) =
            mem.updated(p.head.value, in.applyOrElse(0, (i: Int) => { println("Input needed:"); StdIn.readInt() }))
        override def input(p: List[Parameter])(in: IO): IO =
            if (in.isEmpty) in else in.tail
    }
    case object Output extends Instruction {
        val op = 4
        val nParams = 1
        override def output(p: List[Parameter])(mem: Memory, out: IO) =
            valByAccess(mem, p.head) :: out
    }
    case object JumpIfTrue extends Instruction { 
        val op = 5
        val nParams = 2
        override def moveHead(p: List[Parameter])(mem: Memory, head: Int) = 
            if (valByAccess(mem, p(0)) != 0) valByAccess(mem, p(1))
            else super.moveHead(p)(mem, head)
    }
    case object JumpIfFalse extends Instruction { 
        val op = 6
        val nParams = 2
        override def moveHead(p: List[Parameter])(mem: Memory, head: Int) = 
            if (valByAccess(mem, p(0)) == 0) valByAccess(mem, p(1))
            else super.moveHead(p)(mem, head)
    }
    case object LessThan extends Instruction { 
        val op = 7
        val nParams = 3
        override def exec(p: List[Parameter])(mem: Memory, in: IO) =
            if (valByAccess(mem, p(0)) < valByAccess(mem, p(1))) mem.updated(p(2).value, 1)
            else mem.updated(p(2).value, 0)
    }
    case object Equals extends Instruction { 
        val op = 8
        val nParams = 3
        override def exec(p: List[Parameter])(mem: Memory, in: IO) =
            if (valByAccess(mem, p(0)) == valByAccess(mem, p(1))) mem.updated(p(2).value, 1)
            else mem.updated(p(2).value, 0)
    }
    case object Halt extends Instruction {
        val op = 99
        val nParams = 0
        override def moveHead(p: List[Parameter])(mem: Memory, head: Int) = -1
    }
    
    def getDigitAt(n: Int, pos: Int): Int = 
        (n / Math.pow(10, pos).toInt) % 10
    
    def decode(memoryAtHead: Memory): State => State = {
        val opcode = memoryAtHead.head
        val instr = opcodeMap(opcode % 100)
        val values = memoryAtHead.tail.take(instr.nParams)
        val accesses = (0 until instr.nParams).map(n => getDigitAt(opcode, n + 2)).toList
        val params = (values zip accesses).map(p => Parameter(p._1, p._2))
        
        //println(f"Instruction: ${instr}, params: ${params}")
        
        instr(params)
    } 
    def cycle(initialState: State, stopCondition: State => Boolean): State ={
        def cycleHelper: State => State = {
            case state @ State(mem, head, in, out) => 
                if (!stopCondition(state)) {
                    val decodedInstr = decode(mem.drop(head))
                    val newState = decodedInstr(state)
                    cycleHelper(newState)
                }
                else state
        }

        cycleHelper(initialState)
    }
    def untilHalt = (s: State) => s.head < 0

    def runUntil(mem: Memory, head: Int = 0, in: IO = List(), stopCondition: State => Boolean): State =
        cycle(State(mem, head, in, List()), stopCondition)
    
    def run(mem: Memory, head: Int = 0, in: IO = List()) =
        runUntil(mem, head, in, untilHalt)
    
    def runUntilOutput(mem: Memory, head: Int = 0, in: IO = List()) =
        runUntil(mem, head, in, (s: State) => untilHalt(s) || !s.out.isEmpty)

    val supportedInstr: List[Instruction] = List(Addition, Multiplication, Input, Output, JumpIfTrue, JumpIfFalse, LessThan, Equals, Halt)
    val opcodeMap = supportedInstr.map((i: Instruction) => (i.op, i)).toMap//.withDefault(throw new IllegalArgumentException("Unknown opcode"))
}

// Test the computer here
object Main extends App {
    import IntCode._
    
    val input = Source.fromFile("src/main/resources/day5/input.txt").mkString.stripLineEnd.split(',').toList

    val memory = input.map(_.toInt).toList
    val testMemory = List(
        3,9,8,9,10,9,4,9,99,-1,8
    )

    println(IntCode.run(memory, in=List(5)))
}