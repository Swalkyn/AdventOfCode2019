package day13

import scala.io.Source
import intcode.IntCode
import scala.annotation.tailrec

object Main extends App {
    final val intToPix = Map(
        0 -> ' ',
        1 -> '█',
        2 -> '#',
        3 -> '-',
        4 -> 'o'
    )

    case class Coord(x: Int, y: Int)

	val input = Source.fromFile("src/main/resources/day13/input.txt").mkString
    val memory = IntCode.parseString(input)
    
    val pixels: Map[Coord, Char] = Map.empty.withDefaultValue(' ')
    val output = IntCode.runWith(memory)

    // Part 1
    lazy val finalPixels = output.map(_.toInt).grouped(3).foldLeft(pixels)((ps, out) => ps.updated(Coord(out(0), out(1)), intToPix(out(2))))
    //println(finalPixels.values.count(_ == '#'))
    
    // Part 2
    def display(pixels: Map[Coord, Char], w: Int, h: Int): Unit = {
        (0 until h).map(y => (0 until w).map(x => pixels(Coord(x, y))).mkString).foreach(println)
        println(f"Score: ${pixels(Coord(-1, 0))}")
    }

    @tailrec
    def getBallDir(outputs: LazyList[Long], paddleX: Long): Option[(Long, (LazyList[Long], Long))] = outputs match {
        case ballX #:: _ #:: 4 #:: more => {
            // println("Move paddle")
            val dir = Integer.compare(ballX.toInt, paddleX.toInt)
            Some((dir.toLong, (more, paddleX + dir)))
        }
        case _ #:: _ #:: c #:: more => {
            // println(f"Matched ${intToPix(c.toInt)}")
            getBallDir(more, paddleX)
        }
        case other => None//throw new IllegalArgumentException(f"-> $other")
    }
        
    val modMem = memory.updated(0, 2.toLong)
    lazy val joystick = LazyList.unfold((game, 0): (LazyList[Long], Long)) {
        case (out, paddleX) => getBallDir(out, paddleX)
    }
    lazy val game: LazyList[Long] = IntCode.runWith(modMem, in=LazyList())

    println(joystick.take(10).toList)
    // val gameOutput = game.map(_.toInt).grouped(3).foldLeft(pixels)((ps, out) => ps.updated(Coord(out(0), out(1)), intToPix(out(2))))

    // display(gameOutput, 50, 50)
}
