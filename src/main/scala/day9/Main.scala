package day9

import scala.io.Source
import intcode.IntCode

object Main extends App {
    val input = Source.fromFile("src/main/resources/day9/input.txt")
    val memory = IntCode.parseString(input.mkString)
    val testInput = IntCode.parseString("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")

    def tests = {
        val testInput1 = IntCode.parseString("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
        val testInput2 = IntCode.parseString("1102,34915192,34915192,7,4,7,99,0")
        val testInput3 = IntCode.parseString("104,1125899906842624,99")

        println("Test 1")
        assert(IntCode.runWith(testInput1).toList.toString == "List(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99)")
        println("Test 2")
        assert(IntCode.runWith(testInput2).head.toString.size == 16)
        println("Test 3")
        assert(IntCode.runWith(testInput3).head.toString == "1125899906842624")
    }
    tests

    val part1 = IntCode.runWith(memory, in=LazyList(1))
    println(part1.toList)

    val part2 = IntCode.runWith(memory, in=LazyList(2))
    println(part2.toList)
}
