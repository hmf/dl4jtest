package pt.inescn.examples.chap_0

object chap_0 {
  
  def factorial(x: BigInt): BigInt = if (x == 0) 1 else x * factorial(x - 1)
  
  def main(args: Array[String]): Unit = {
    var capital = Map("US" -> "Washington", "France" -> "Paris")
    capital += ("Japan" -> "Tokyo")
    println(capital("France"))
    
    println(factorial(30))
  }
}