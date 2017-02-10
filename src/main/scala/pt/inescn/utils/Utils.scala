package pt.inescn.utils

object Utils {
 
  /**
   * Used for timing a single call.
   */
  def time[ R ]( block: => R ): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    //println( "Elapsed time: " + ( t1 - t0 ) + "ns" )
    println( "Elapsed time: " + ( t1 - t0 ) / 1e9+ "sec" )
    result
  }
  
  /**
   * Compare two real values with a given precision. 
   */
  def aproxEqual( a: Double, b: Double, eps: Double = 0.000001 ) = ( ( b + eps ) >= a ) && ( a >= ( b - eps ) )
  
}