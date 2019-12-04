package day3

import scala.io.Source

object Main extends App {
    type Segment = (Direction, Int)
    type Coord = (Int, Int)
    
    sealed trait Direction { 
        val hops, vops: (Int, Int) => Int
        def keepCoord = (c: Int, s: Int) => c
    }

    case object Up extends Direction { val hops = keepCoord; val vops = _ + _ }
    case object Down extends Direction { val hops = keepCoord; val vops = _ - _ }
    case object Left extends Direction { val hops = _ - _; val vops = keepCoord }
    case object Right extends Direction { val hops = _ + _; val vops = keepCoord }
    
    sealed trait Orientation { val constCoord: Coord => Int; val diffCoord: Coord => Int}
    case object H extends Orientation { val constCoord = _._2; val diffCoord = _._1 }
    case object V extends Orientation { val constCoord = _._1; val diffCoord = _._2 }
    
    case class Line(val s: Coord, val e: Coord, val o: Orientation) {
        def intersect = (other: Line) =>
             ((((this.s._1 <= other.s._1 && other.s._1 <= this.e._1) || (this.e._1 <= other.s._1 && other.s._1 <= this.s._1)) 
            || ((this.s._1 <= other.e._1 && other.e._1 <= this.e._1) || (this.e._1 <= other.e._1 && other.e._1 <= this.s._1))) 
            &&(((other.s._2 <= this.s._2 && this.s._2 <= other.e._2) || (other.e._2 <= this.s._2 && this.s._2 <= other.s._2)) 
            || ((other.s._2 <= this.e._2 && this.e._2 <= other.e._2) || (other.e._2 <= this.e._2 && this.e._2 <= other.s._2)))) ||
            ((((this.s._2 <= other.s._2 && other.s._2 <= this.e._2) || (this.e._2 <= other.s._2 && other.s._2 <= this.s._2)) 
            || ((this.s._2 <= other.e._2 && other.e._2 <= this.e._2) || (this.e._2 <= other.e._2 && other.e._2 <= this.s._2))) 
            &&(((other.s._1 <= this.s._1 && this.s._1 <= other.e._1) || (other.e._1 <= this.s._1 && this.s._1 <= other.s._1)) 
            || ((other.s._1 <= this.e._1 && this.e._1 <= other.e._1) || (other.e._1 <= this.e._1 && this.e._1 <= other.s._1))))

        def closestIntersection(other: Line): Option[Coord] =
            if (this.intersect(other)) {
                if (this.o == other.o) {
                    val f = this.o.diffCoord
                    val (lower, upper) = (
                        Integer.max(Integer.min(f(this.s), f(this.e)), Integer.min(f(other.s), f(other.e))),
                        Integer.min(Integer.max(f(this.s), f(this.e)), Integer.max(f(other.s), f(other.e)))
                    )
                    Some((this.o.constCoord(this.s), closestToZero(lower, upper)))
                }
                else Some((this.o.constCoord(this.s), other.o.constCoord(other.s)))
            }
            else None
        
        def closestToZero(lower: Int, upper: Int): Int =
            if (lower <= 0 && upper >= 0) 0
            else if (lower > 0) lower
            else upper
    }
    
    val testInput = List("R8,U5,L5,D3", "U7,R6,D4,L4")
	val input = Source.fromFile("src/main/resources/day3/input.txt").getLines().toList
    val wires = input.map(_.split(",").toList.map(parseSegments(_)))
    
    def parseSegments(d: String): Segment =
        d.splitAt(1) match {
            case ("U", l) => (Up, l.toInt)
            case ("D", l) => (Down, l.toInt)
            case ("L", l) => (Left, l.toInt)
            case ("R", l) => (Right, l.toInt)
            case _ => throw new IllegalArgumentException("Incorrectly formatted file")
        }

    def orientationFromDirection(d: Direction): Orientation =
        d match {
            case Up | Down => V
            case Left | Right => H
        }

    def lineFromSegment(c: Coord, s: Segment): Line =
        Line(c, (s._1.hops(c._1, s._2), s._1.vops(c._2, s._2)), orientationFromDirection(s._1))

    val wireAsLines = wires.map(_.foldLeft(List(Line((0, 0), (0, 0), H)))
        ((acc, s) => lineFromSegment(acc.head.e, s) :: acc).reverse.drop(1))

    val intersections = wireAsLines(0).flatMap(l1 => wireAsLines(1).flatMap(l2 => l1.closestIntersection(l2)))

    val withDistances = intersections.map(c => (c, Math.abs(c._1) + Math.abs(c._2))).sortBy(_._2).drop(1)
    
	println(withDistances.take(10))
}
