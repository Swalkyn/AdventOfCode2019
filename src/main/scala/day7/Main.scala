package day7

import scala.io.Source
import intcode.IntCode
import intcode.IntCode.State

object Main extends App {
	val input = Source.fromFile("src/main/resources/day7/input.txt").mkString.stripLineEnd.split(',').toList
    val memory = input.map(_.toInt).toList
    val testMemory = List(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
    
    // Part 1
    val part1 = (0 to 4).permutations.map(_.foldLeft(0)(
        (acc, phase) => IntCode.runUntilHalt(IntCode.getStateFor(memory, in=List(phase, acc))).out.head)
    ).max
    
    println(part1)

    // Part 2
    val permutations = (5 to 9).permutations.map(_.map(phase => IntCode.getStateFor(memory, in=List(phase))).toList)

    def propagateSignal(states: List[State], signal: Int, i: Int): Int = states(i) match {
        case State(_, -1, _, _) => signal
        case State(mem, head, in, out) => {
            println(f"Running computer $i")
            val newState = IntCode.runUntilOutput(State(mem, head, if (in.isEmpty) List(signal) else List(in.head, signal), List()))
            val newSignal = if (newState.out.isDefinedAt(0)) newState.out.head else signal
            propagateSignal(states.updated(i, newState), newSignal, (i+1)%5)
        }
    }

    val part2 = permutations.map(propagateSignal(_, 0, 0)).max

    println(part2)
}
