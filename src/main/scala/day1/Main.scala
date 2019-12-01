package day1

import scala.io.Source

object Main extends App {
	val input = Source.fromFile("src/main/resources/day1/input.txt").getLines().toList
    
    // Part 1
    println(input.map(_.toInt / 3 - 2).sum)

    // Part 2
    def positivelyBound = (x: Int) => if (x > 0) x else 0

    def fuelRequirements(mass: Int, total: Int): Int = {
        val fuel = positivelyBound(mass / 3 - 2)
        if (fuel == 0) total
        else fuelRequirements(fuel, total + fuel)
    }

    println(input.map(m => fuelRequirements(m.toInt, 0)).sum)
}
