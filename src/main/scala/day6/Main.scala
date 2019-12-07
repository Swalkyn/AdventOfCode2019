package day6

import scala.io.Source
import scala.util.matching.Regex


object Main extends App {
    val input = Source.fromFile("src/main/resources/day6/input.txt").getLines().toList
    val testInput = List("COM)B", "B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L")
    
    val orbitFormat = raw"(.+)\)(.+)".r
    implicit val orbitMap = input.map(
        _ match {
            case orbitFormat(o1, o2) => (o2, o1)
            case _ => throw new IllegalArgumentException("Badly formatted file")
        }
    ).toMap

    
    def orbitsBetween(start: String, target: String)(implicit orbitMap: Map[String, String]): Int = {
        def orbitsHelper(body: String, total: Int): Int =
            if (body == target) total
            else orbitsHelper(orbitMap(body), total + 1)
        
        orbitsHelper(start, 0)
    }

    def orbitPathBetween(start: String, target: String)(implicit orbitMap: Map[String, String]): List[String] = {
        def orbitsPathHelper(body: String, path: List[String]): List[String] =
            if (body == target) path.reverse
            else orbitsPathHelper(orbitMap(body), body :: path)
        
        orbitsPathHelper(start, List())
    }
    
    // Part 1
    val part1 = orbitMap.keySet.toList.map(orbitsBetween(_, "COM")).sum
    
    println(part1)

    // Part 2
    val youPath = orbitPathBetween("YOU", "COM")
    val santaPath = orbitPathBetween("SAN", "COM")
    val topOrbit = youPath.find(santaPath.contains(_)).get
    val part2 = orbitsBetween("YOU", topOrbit) + orbitsBetween("SAN", topOrbit) - 2

    println(part2)
}
