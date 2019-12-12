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
        (acc, phase) => IntCode.run(IntCode.getStateFor(memory, in=LazyList(phase, acc))).head)
    ).max
    
    println(part1)

    // Part 2
    /*
    Using lazy lists in such a clever way does not come from me, but
    https://github.com/sim642/adventofcode/blob/master/src/main/scala/eu/sim642/adventofcode2019/Day7.scala,
    which apparently is also inspired from somebody else.
     */
    val settings: List[List[State]] = (5 to 9).permutations.map(_.map(phase => IntCode.getStateFor(memory, in=LazyList(phase))).toList).toList
    
    def getSignalFinalValueFor(setting: List[State]) = {
        def propagatedSignal: LazyList[Int] = setting.foldLeft(0 #:: propagatedSignal)(
            (input, initState) => IntCode.run(initState.copy(in=initState.in.head #:: input)))

        propagatedSignal.last
    }

    val part2 = settings.map(getSignalFinalValueFor(_)).max
    println(part2)

}
