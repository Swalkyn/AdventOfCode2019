package intcode

import scala.io.Source
import scala.io.StdIn

/*
Possible improvements
 - use a map for the s.memory, for faster updates
 - use a lazylist with unfold instead of cycle
 - redesign IO
*/

object IntCode {
    type Memory = List[Int]
    type PC = Int
    type IO = List[Int]

    case class Parameter(value: Int, access: Int)
    case class State(mem: Memory, head: PC, rel: PC, in: IO, out: IO)

    sealed trait Instruction {
        val op: Int
        val nParams: Int
        def valByAccess = (s: State, p: Parameter) => p.access match {
            case 0 => s.mem(p.value)
            case 1 => p.value
            case 2 => s.mem(s.rel + p.value)
            case _ => throw new IllegalArgumentException
        }

        def apply(p: List[Parameter])(s: State): Option[State] =
                halted(State(exec(p)(s), moveHead(p)(s), moveRBase(p)(s), input(p)(s), output(p)(s)))
        
        def halted(s: State): Option[State] = Some(s)
        def exec(p: List[Parameter])(s: State): Memory = s.mem
        def moveHead(p: List[Parameter])(s:State): PC = s.head + nParams + 1
        def moveRBase(p: List[Parameter])(s: State): PC = s.rel
        def input(p: List[Parameter])(s: State): IO = s.in
        def output(p: List[Parameter])(s: State): IO = s.out
    }
    case object Addition extends Instruction { 
        val op = 1
        val nParams = 3
        override def exec(p: List[Parameter])(s: State) =
            s.mem.updated(p(2).value, valByAccess(s, p(0)) + valByAccess(s, p(1)))
    }
    case object Multiplication extends Instruction { 
        val op = 2
        val nParams = 3
        override def exec(p: List[Parameter])(s: State) =
            s.mem.updated(p(2).value, valByAccess(s, p(0)) * valByAccess(s, p(1)))
    }
    case object Input extends Instruction {
        val op = 3
        val nParams = 1
        override def exec(p: List[Parameter])(s: State) =
            s.mem.updated(p.head.value, s.in.applyOrElse(0, (i: Int) => { println("Input needed:"); StdIn.readInt() }))
        override def input(p: List[Parameter])(s: State): IO =
            if (s.in.isEmpty) s.in else s.in.tail
    }
    case object Output extends Instruction {
        val op = 4
        val nParams = 1
        override def output(p: List[Parameter])(s: State) =
            valByAccess(s, p.head) :: s.out
    }
    case object JumpIfTrue extends Instruction { 
        val op = 5
        val nParams = 2
        override def moveHead(p: List[Parameter])(s: State) = 
            if (valByAccess(s, p(0)) != 0) valByAccess(s, p(1))
            else super.moveHead(p)(s)
    }
    case object JumpIfFalse extends Instruction { 
        val op = 6
        val nParams = 2
        override def moveHead(p: List[Parameter])(s: State) = 
            if (valByAccess(s, p(0)) == 0) valByAccess(s, p(1))
            else super.moveHead(p)(s)
    }
    case object LessThan extends Instruction { 
        val op = 7
        val nParams = 3
        override def exec(p: List[Parameter])(s: State) =
            if (valByAccess(s, p(0)) < valByAccess(s, p(1))) s.mem.updated(p(2).value, 1)
            else s.mem.updated(p(2).value, 0)
    }
    case object Equals extends Instruction { 
        val op = 8
        val nParams = 3
        override def exec(p: List[Parameter])(s: State) =
            if (valByAccess(s, p(0)) == valByAccess(s, p(1))) s.mem.updated(p(2).value, 1)
            else s.mem.updated(p(2).value, 0)
    }
    case object RBase extends Instruction {
        val op = 9
        val nParams = 1
        override def moveRBase(p: List[Parameter])(s: State) = 
            s.rel + p.head.value
    }
    case object Halt extends Instruction {
        val op = 99
        val nParams = 0
        override def halted(s: State) = None
    }
    
    def getDigitAt(n: Int, pos: Int): Int = 
        (n / Math.pow(10, pos).toInt) % 10
    
    def decode(memoryAtHead: Memory): State => Option[State] = {
        val opcode = memoryAtHead.head
        val instr = opcodeMap(opcode % 100)
        val values = memoryAtHead.tail.take(instr.nParams)
        val accesses = (0 until instr.nParams).map(n => getDigitAt(opcode, n + 2)).toList
        val params = (values zip accesses).map(p => Parameter(p._1, p._2))
        
        // println(f"Instruction: ${instr}, params: ${params}")
        
        instr(params)
    }

    def run(initialState: State) = {
        def cycle: Option[State] => Option[State] = 
            _.flatMap(s => decode(s.mem.drop(s.head))(s))
        
        LazyList.iterate[Option[State]](Some(initialState))(cycle).takeWhile(_ != None).flatten
    }

    def runUntilHalt(s: State): State =
        run(s).last
    
    def runUntilOutput(s: State): State =
        run(s).takeWhile(_.out.isEmpty).last
    
    def runNumber(s: State, n: Int) = {
        run(s).take(n).last
    }
    
    def getStateFor(mem: Memory, head: PC = 0, in: IO = List()) =
        State(mem, head, 0, in, List())
        
    val supportedInstr: List[Instruction] = List(Addition, Multiplication, Input, Output, JumpIfTrue, JumpIfFalse, LessThan, Equals, Halt)
    val opcodeMap = supportedInstr.map((i: Instruction) => (i.op, i)).toMap//.withDefault(throw new IllegalArgumentException("Unknown opcode"))
}

// Test the computer here
object Main extends App {
    import IntCode._
    
    val input = Source.fromFile("src/main/resources/day5/input.txt").mkString.stripLineEnd.split(',').toList

    val memory = input.map(_.toInt).toList
    val testmemory = List(
        3,9,8,9,10,9,4,9,99,-1,8
        )
    lazy val result = IntCode.runUntilHalt(IntCode.getStateFor(memory, in=List(5)))//foreach(s => println(s.mem))
    println(result)
}