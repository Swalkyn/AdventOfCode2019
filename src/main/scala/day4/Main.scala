package day4

object Main extends App {
    val input = (172930 to 683082)
    
    def hasIncreasingDigits(n: Int): Boolean = 
    if (n < 10) true
    else if ((n % 10) < (n / 10) % 10) false
    else hasIncreasingDigits(n / 10)
    
    def twoNotThree(s: String): Boolean =
        s(0) == s(1) && s(1) != s(2)

    def hasDoubleAdjDigits(n: Int) = 
        n.toString.sliding(2).exists(s => s(0) == s(1))
    
    def hasStrictlyDoubleAdjDigits(n: Int) = {
        val s = n.toString
        (s.sliding(4).exists(s => s(0) != s(1) && s(1) == s(2) && s(2) != s(3))
         || twoNotThree(s.take(3))
         || twoNotThree(s.takeRight(3).reverse))
    } 
    
    val part1 = input.filter(n => hasIncreasingDigits(n) && hasDoubleAdjDigits(n)).size
    val part2 = input.filter(n => hasIncreasingDigits(n) && hasStrictlyDoubleAdjDigits(n)).size
    
    println(part1)
    println(part2)
}
