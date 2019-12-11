package day10

import scala.io.Source

object Main extends App {
    val input = Source.fromFile("src/main/resources/day10/input.txt").getLines()
    val testInput = List("###", "###", "###")//List(".#..#", ".....", "#####", "....#", "...##")

    case class Coord(x: Int, y: Int) {
        def angle: Double = {
            Math.atan2(x, y)
        }

        def dist: Double = {
            Math.sqrt(x*x + y*y)
        }

        def relativeTo(source: Coord): Coord =
            Coord(this.x - source.x, this.y - source.y)
    }


    val asteroids = (for {
        (l, y) <- input.zipWithIndex
        (c, x) <- l.zipWithIndex
    } yield if (c == '#') Some(Coord(x, y)) else None).flatten.toList


    def observableFrom(source: Coord, asteroids: List[Coord]): Int = {
        asteroids.filter(_ != source).map(a => a.relativeTo(source).angle).distinct.count(c => true)
    }

    // Part 1
    val (station, observable) = asteroids.map(s => (s, observableFrom(s, asteroids))).maxBy(_._2)
    println(station)
    println(observable)

    // Part 2

    /*
    The following operation
    1. remove asteroid station (won't be destroyed)
    2. group asteroids on same firing line together
    3. assign them an index depending on their position. 0 is closest.
    4. (transform the map to a list)
    5. flatten and put all asteroids together with a tuple of minus their position and their angle
    6. sort by this (first by furthest position and then by furthest angle). We have to this because indices start at zero
    7. reverse it
    8. only keep coordinates
     */
    val destructionOrder = asteroids
        .filter(_ != station)
        .groupBy(_.relativeTo(station).angle)
        .mapValues(_.sortBy(_.relativeTo(station).dist).zipWithIndex)
        .toList
        .flatMap { case (angle, asteroids) => asteroids.map { case (a, i) => (a, (-i, angle)) } }
        .sortBy(_._2)
        .reverse
        .map(_._1)

    val part2 = destructionOrder(199).x * 100 + destructionOrder(199).y
    println(part2)
}
