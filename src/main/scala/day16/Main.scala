package day16

import scala.io.Source
import scala.annotation.tailrec

object Main extends App {
	val input = Source.fromFile("src/main/resources/day16/input.txt").toList.map(_.asDigit)
	val testInput = List(0, 3, 0, 3, 6, 7, 3, 2, 5, 7, 7, 2, 1, 2, 9, 4, 4, 0, 6, 3, 4, 9, 1, 5, 6, 5, 4, 7, 4, 6, 6, 4)

	val pattern = List(1, 0, -1, 0)

	def fft(input: List[Int]): List[Int] =
		input.zipWithIndex.map {
			case (x, i) if i >= input.size/2 => input.drop(i).sum % 10
			case (x, i) => input.drop(i).zipWithIndex.map {
				case (y, j) => y * pattern((j / (i+1)) % pattern.size)
			}.sum % 10
		}.map(Math.abs)
	
	def fft2(input: List[Int]): List[Int] =
		input.foldLeft((input.sum, List[Int]())) {
			case ((s, out), x) => (s - x, Math.abs(s % 10) :: out)
		}._2.reverse

	@tailrec
	def repeatedFFT(input: List[Int], n: Int): List[Int] =
		if (n == 0) input
		else repeatedFFT(fft2(input), n-1)

	// Part 1
	println(repeatedFFT(input, 100).take(8))

	// Part 2
	val input2 = List.iterate(input, 10000)(identity).flatten
	val offset = input2.take(7).mkString.toInt
	println(offset)
	println(repeatedFFT(input2, 100).drop(offset).take(8))
}
