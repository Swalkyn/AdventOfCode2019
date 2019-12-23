package day15

import scala.io.Source
import intcode.IntCode

object Main extends App {
	val input = Source.fromFile("src/main/resources/day15/input.txt").mkString
	val memory = IntCode.parseString(input)
	val moveToChar = Map(
		0 -> '█',
		1 -> '.',
		2 -> 'x',
		3 -> ' '
	)
	
	case class Point(x: Int, y: Int) {
		def go(command: Int) = command match {
			case 1 => Point(x, y+1)
			case 2 => Point(x, y-1)
			case 3 => Point(x-1, y)
			case 4 => Point(x+1, y)
		} 
	}

	def printGrid(grid: Map[Point, Int]): Unit = {
        val (minX, maxX) = (grid.keys.map(_.x).min, grid.keys.map(_.x).max)
        val (minY, maxY) = (grid.keys.map(_.y).min, grid.keys.map(_.y).max)
        (for {
            y <- (-maxY to -minY)
        } yield (for {
            x <- (minX to maxX)
        } yield moveToChar(grid(Point(x, -y)))).mkString).foreach(println)
    }
	
	def searchWith(point: Point, visited: Map[Point, (Int, Int)], robot: IntCode.State, depth: Int): Map[Point, (Int, Int)] = {
		if (visited.contains(point) && visited(point)._2 <= depth) Map()
		else if (robot.out.get != 1)
		Map(point -> (robot.out.get.toInt, depth))
		else
		(1 to 4).foldLeft(visited + (point -> (robot.out.get.toInt, depth)))(
			(acc, c) => acc ++ searchWith(point.go(c), acc, IntCode.step(robot.copy(in=LazyList(c))), depth+1)
			)
		}
	
	def oxygenFill(layout: Map[Point, (Int, Int)]): Int = {
		def oxygenFillHelper(expanded: Set[Point], expanding: Set[Point], time: Int): Int =
			if (expanding.isEmpty || time > 4000) time-1
			else {
				val newExpanding = expanding
					.flatMap(p => (1 to 4).map(c => p.go(c))
					.filter(p => layout(p)._1 != 0 && !expanded.contains(p)))
				oxygenFillHelper(expanded ++ expanding, newExpanding, time+1)
			}

		oxygenFillHelper(Set(), Set(oxygenSys(layout)._1), 0)
	}
		
	def oxygenSys(layout: Map[Point, (Int, Int)]) = layout.filter(_._2._1 == 2).head
	lazy val roomLayout = searchWith(Point(0, 0), Map(), IntCode.State(memory, 0, 0, LazyList(), Some(1)), 0)
		
	// Part 1
	val part1 = oxygenSys(roomLayout)
	println(part1._2._2)
	printGrid(roomLayout.mapValues(_._1).toMap.withDefaultValue(3))

	// Part 2
	val part2 = oxygenFill(roomLayout)
	println(part2)
}
