package day2

import scala.io.Source

object Main extends App {
    val testInput = List(1,9,10,3,2,3,11,0,99,30,40,50)
	val input = Source.fromFile("src/main/resources/day2/input.txt").mkString.split(',').toList

    def exec(mem: List[Int], instr: List[Int]): (Boolean, List[Int]) =
        instr match {
            case op :: a :: b :: dest :: more =>
                if (op == 1) (true, mem.updated(dest, mem(a)+mem(b)))
                else if (op == 2) (true, mem.updated(dest, mem(a)*mem(b)))
                else if (op == 99) (false, mem)
                else throw new IllegalArgumentException("Unkown instruction")
            case op :: more =>
                if (op == 99) (false, mem)
                else throw new IllegalArgumentException("Ran out of instructions without ever halting")
            case _ => throw new IllegalArgumentException("Unknown error")
        }

    /*
    This version of run might be easier to understand, but is less elegant in my opinion

    def run(mem: List[Int], start: Int): Int = 
        exec(mem, mem.splitAt(start)._2) match {
            case (true, newMem) => run(newMem, start+4)
            case (false, _) => mem(0)
        }
    */
    def run(mem: List[Int]) = 
        mem
            .sliding(4, 4)
            .foldLeft((true, mem))((state, i) => if (state._1) exec(state._2, i) else state)
            ._2(0)

    def runWith(mem: List[Int], noun: Int, verb: Int): Int =
        run(mem.updated(1, noun).updated(2, verb))
    
    def findInputsFor(mem: List[Int], result: Int) = {
        def diagGridSearch(noun: Int, total: Int): (Int, Int) =
            if (runWith(mem, noun, total - noun) == result) (noun, total - noun)
            else if (noun - total == 0) diagGridSearch(0, total + 1)
            else diagGridSearch(noun + 1, total)
     
        diagGridSearch(0, 0)
    }

    val memory = input.map(_.toInt).toList

    // Part 1
    println(runWith(memory, 12, 2))

    // Part 2
    val (noun, verb) = findInputsFor(memory, 19690720)
    println(100 * noun + verb)
}
