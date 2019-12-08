package day8

import scala.io.Source

object Main extends App {
	val input = Source.fromFile("src/main/resources/day8/input.txt").toList.map(_.asDigit)
    val testInput = "0222112222120000".toList.map(_.asDigit)

    val (w, h) = (25, 6)
    val pixels = w*h
    val printMap = Map(
        0 -> ' ',
        1 -> '█',
        2 -> '~'
    )

    val layers = input.grouped(pixels).toList
        
    // Part 1
    val mostZeroes = layers
        .map(_.groupBy(identity)
        .withDefaultValue(List())
        .mapValues(_.size))
        .minBy(_.get(0))

    val part1 = mostZeroes(1) * mostZeroes(2)

    println(part1)
    
    // Part 2
    val stacked = layers.foldLeft(List.fill(pixels)(2))(
        (acc, layer) => (acc zip layer).map{ case(above, below) => if (above == 2) below else above }
    )

    val part2 = stacked.map(printMap(_)).grouped(w).map(_.mkString)
    part2.foreach(println)
}
