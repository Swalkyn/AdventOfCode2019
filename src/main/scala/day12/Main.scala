package day12

import scala.io.Source

object Main extends App {

    case class Vect3D(x: Int, y: Int, z: Int) {
        def add(other: Vect3D) =
            Vect3D(this.x + other.x, this.y + other.y, this.z + other.z)

        // This methods works under the assumption that compare returns either -1, 0 or 1
        def gravityChange(other: Vect3D) =
            Vect3D(
                Ordering.Int.compare(other.x, this.x),
                Ordering.Int.compare(other.y, this.y),
                Ordering.Int.compare(other.z, this.z)
            )

        def energy =
            Math.abs(x) + Math.abs(y) + Math.abs(z)
    }

    case class Moon(pos: Vect3D, vel: Vect3D, id: Int) {
        def energy = 
            pos.energy * vel.energy

        def updatePos = 
            Moon(pos.add(vel), vel, id)
        
        def addInfluenceFrom(other: Moon) =
            Moon(pos, this.vel.add(this.pos.gravityChange(other.pos)), id)
    }

    def updateSystem(sys: List[Moon]): List[Moon] =
        sys.map(m => sys.filter(_ != m).fold(m)((mA, mB) => mA.addInfluenceFrom(mB))).map(_.updatePos)

    val input = Source.fromFile("src/main/resources/day12/input.txt").getLines()
    val positionString = raw"<x=(.+), y=(.+), z=(.+)>".r

    val startingPositions: List[Vect3D] = input.map { s =>
        s match {
            case positionString(x, y, z) => Vect3D(x.toInt, y.toInt, z.toInt)
            case _ => throw new IllegalArgumentException("Incorrect position format")
        }
    }.toList

    val startingSystem = startingPositions.zipWithIndex.map { case (pos, i) => Moon(pos, Vect3D(0, 0, 0), i) }
    val evolvingSytem = LazyList.iterate(startingSystem)(updateSystem)

    // Part 1
    //evolvingSytem.take(10 + 1).toList.foreach(s => println(s.sortBy(_.id)))
    println(evolvingSytem(1000).map(_.energy).sum)
}
