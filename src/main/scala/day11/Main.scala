package day11

import scala.io.Source
import intcode.IntCode
import scala.io.StdIn

object Main extends App {
    
    case class Panel(x: Int, y: Int) {
        def move(d: Direction) = d match {
            case Down => Panel(x, y-1)
            case Left => Panel(x-1, y)
            case Right => Panel(x+1, y)
            case Up => Panel(x, y+1)
        }
    }
    
    sealed trait Direction {
        def turnLeft: Direction
        def turnRight: Direction
    }
    case object Up extends Direction {
        def turnLeft: Direction = Left
        def turnRight: Direction = Right
    }
    case object Down extends Direction {
        def turnLeft: Direction = Right
        def turnRight: Direction = Left
    }
    case object Right extends Direction {
        def turnLeft: Direction = Up
        def turnRight: Direction = Down
    }
    case object Left extends Direction {
        def turnLeft: Direction = Down
        def turnRight: Direction = Up
    }
    
    case class OrientedPosition(panel: Panel, dir: Direction) {
        def turnAndMove(turn: Int): OrientedPosition =
        if (turn == 0) OrientedPosition(panel.move(dir.turnLeft), dir.turnLeft)
        else if (turn == 1) OrientedPosition(panel.move(dir.turnRight), dir.turnRight)
        else throw new IllegalArgumentException
    }
    
    def printHull(paintedPanels: Map[Panel, (Long, Boolean)]): Unit = {
        val (minX, maxX) = (paintedPanels.keys.map(_.x).min, paintedPanels.keys.map(_.x).max)
        val (minY, maxY) = (paintedPanels.keys.map(_.y).min, paintedPanels.keys.map(_.y).max)
        (for {
            y <- (-maxY to -minY)
        } yield (for {
            x <- (minX to maxX)
        } yield if (paintedPanels(Panel(x, -y))._1 == 0) ' ' else '█').mkString).foreach(println)
    }
    
        val input = Source.fromFile("src/main/resources/day11/input.txt").mkString
    
    val robotMemory = IntCode.parseString(input)
    val panelInfo: Map[Panel, (Long, Boolean)] = Map.empty.withDefaultValue((0, false))
    val initialPos = OrientedPosition(Panel(0, 0), Up)

    def paint(initialColor: Long): Map[Panel, (Long, Boolean)] = {
        lazy val robotOutput: LazyList[Long] = IntCode.runWith(robotMemory, in=cameraOutput)
        lazy val paintProcedure = LazyList.unfold((panelInfo, robotOutput, initialPos): (Map[Panel, (Long, Boolean)], LazyList[Long], OrientedPosition)) {
            case (panels, out, pos) =>
                out match {
                    case color #:: turn #:: more => {
                        // println(f"Robot paints $color and turns $turn")
                        val newPanels = panels.updated(pos.panel, (color, true))
                        val newPos = pos.turnAndMove(turn.toInt)
                        val newColor = newPanels(newPos.panel)._1
                        Some(((newColor, newPanels), (newPanels, more, newPos)))
                    }
                    case other => {
                        None
                    }
                }
        }
    
        lazy val cameraOutput: LazyList[Long] = initialColor #:: paintProcedure.map(_._1)
        lazy val paintedPanels: LazyList[Map[Panel, (Long, Boolean)]] = paintProcedure.map(_._2)

        paintedPanels.last
    }

    // Part 1
    val part1 = paint(0)

    printHull(part1)
    println(part1.count { case(p, (color, painted)) => painted })

    // Part 2
    val part2 = paint(1)
    printHull(part2)
}
