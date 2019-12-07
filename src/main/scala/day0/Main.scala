package day0

import scala.io.Source

sealed trait Tile
case object Tree extends Tile { override def toString: String = "|" }
case object Open extends Tile { override def toString: String = "." }
case object Lumb extends Tile { override def toString: String = "#" }
case object None extends Tile { override def toString: String = " " }

class Instance(init: Iterator[String], size: Int) {
    type Row = List[Tile]
    type Field = List[Row]

    val tiles: Field = init.map(_.map(charToTile).toList).toList
    val paddingRow: Row = List.fill(size + 2)(None)

    def padField(f: Field): Field =
        (paddingRow :: f.map((l: Row) => (None :: l) :+ None)) :+ paddingRow

    def charToTile(c: Char): Tile = c match {
        case '.' => Open
        case '#' => Lumb
        case '|' => Tree
    }
    
    def tick(f: Field): Field =
        padField(f).sliding(3).map(rowTick(_)).toList

    def rowTick(subfield: Field): Row = {
        val regions = subfield.map(_.sliding(3).toList).transpose
        regions.map(r => tileTick(r.flatten))
    }

    def tileTick(region: List[Tile]): Tile = {
        val count = region.groupBy(identity).withDefaultValue(List()).mapValues(_.size)
        region(4) match {
            case Open => if (count(Tree) >= 3) Tree else Open
            case Tree => if (count(Lumb) >= 3) Lumb else Tree
            case Lumb => if (count(Lumb) >= 2 && count(Tree) >= 1) Lumb else Open
            case None => throw new IllegalArgumentException
        }
    }

    def run(ticks: Int): Field = {
        def runHelper(ticks: Int, f: Field): Field = {
            if (ticks > 0) runHelper(ticks - 1, tick(f))
            else f
        }
        runHelper(ticks, tiles)
    }

    def runAndValue(ticks: Int): List[Int] = {
        def runAndValueHelper(ticks: Int, f: Field, values: List[Int]): List[Int] = {
            if (ticks > 0) {
                val newTiles = tick(f)
                val count = newTiles.flatten.groupBy(identity).withDefaultValue(List()).mapValues(_.size)
                runAndValueHelper(ticks - 1, newTiles, count(Tree) * count(Lumb) :: values)
            }
            else values.reverse
        }
        runAndValueHelper(ticks, tiles, List())
    }
}

object Main extends App {
	val input = Source.fromFile("src/main/resources/day0/input.txt").getLines()
    
    val problem = new Instance(input, 50)
    val solution = problem.run(10000)
    println(solution.map(_.mkString).mkString("\n"))

    val count = solution.flatten.groupBy(identity).withDefaultValue(List()).mapValues(_.size)
    println(count(Tree) * count(Lumb))

    println(problem.runAndValue(1000))
}
