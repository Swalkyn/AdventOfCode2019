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
    type Memory = Map[Int, Int]
    type PC = Int

    case class Parameter(value: Int, access: Int)
    case class State(mem: Memory, head: PC, rel: PC, in: LazyList[Int], out: Option[Int])

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
        def input(p: List[Parameter])(s: State): LazyList[Int] = s.in
        def output(p: List[Parameter])(s: State): Option[Int] = None
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
        override def input(p: List[Parameter])(s: State): LazyList[Int] =
            if (s.in.isEmpty) s.in else s.in.tail
    }
    case object Output extends Instruction {
        val op = 4
        val nParams = 1
        override def output(p: List[Parameter])(s: State) =
            Some(valByAccess(s, p.head))
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
    
    def exec(s: State): Option[State] = {
        val opcode = s.mem(s.head)
        val instr = opcodeMap(opcode % 100)
        val values = (1 to instr.nParams).map(n => s.mem(s.head + n))
        val accesses = (0 until instr.nParams).map(n => getDigitAt(opcode, n + 2)).toList
        val params = (values zip accesses).map(p => Parameter(p._1, p._2)).toList
        
        //println(f"Instruction: ${instr}, params: ${params}")
        
        instr(params)(s)
    }

    def run(initialState: State): LazyList[Int] = {
        def cycle: State => Option[(Option[Int], State)] = 
            exec(_).flatMap(s => Some((s.out, s)))

        LazyList.unfold(initialState)(cycle).flatten
    }
    
    def getStateFor(mem: Seq[Int], head: PC = 0, in: LazyList[Int] = LazyList()) =
        State(mem.zipWithIndex.map(_.swap).toMap, head, 0, in, None)
        
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
    val initState = IntCode.getStateFor(memory, in=LazyList(5))
    lazy val result = IntCode.run(initState)//foreach(s => println(s.mem))
    result.foreach(println)
}