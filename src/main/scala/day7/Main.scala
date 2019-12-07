package day7

import scala.io.Source
import intcode.IntCode

object Main extends App {
	val input = Source.fromFile("src/main/resources/day7/input.txt").mkString.stripLineEnd.split(',').toList
    val memory = input.map(_.toInt).toList
    
    // Part 1
    val part1 = (0 to 4).permutations.map(_.foldLeft(0)((acc, phase) => IntCode.run(memory, in=List(phase, acc)).head)).max
    
    println(part1)

    // Part 2    
}
