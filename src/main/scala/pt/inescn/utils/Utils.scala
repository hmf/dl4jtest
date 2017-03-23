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
    println( "Elapsed time: " + ( t1 - t0 ) / 1e9 + "sec" )
    result
  }

  def makeData(year : Short, month : Short, date : Short, hrs : Short, min : Short,  sec : Short = 0, milli : Short = 0) = {
    import java.util.Calendar 

    val cal = Calendar.getInstance()
    cal.clear()
    cal.set( Calendar.YEAR, year )
    cal.set( Calendar.MONTH, month - 1 )
    cal.set( Calendar.DATE, date )
    cal.set( Calendar.HOUR, hrs )
    cal.set( Calendar.MINUTE, min )
    cal.set( Calendar.SECOND, sec )
    cal.set( Calendar.MILLISECOND, milli )

    cal.getTime()
  }
}