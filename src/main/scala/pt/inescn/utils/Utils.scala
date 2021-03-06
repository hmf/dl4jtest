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

  val zoneID_UTC = java.time.ZoneId.of( "UTC" )

  /**
   * Utility function to generate a `java.util.Date` used prior to JDK 8.
   * For example: json4s.orf still uses the `java.util.Date` date.
   */
  def makeData( year: Short, month: Short, date: Short, hrs: Short, min: Short, sec: Short = 0, milli: Short = 0 ) = {
    import java.util.Calendar
    import java.util.TimeZone

    val cal = Calendar.getInstance( TimeZone.getTimeZone( "UTC" ) )
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

  def makeInstance( year: Short, month: Short, day: Short, hrs: Short, min: Short, sec: Short = 0, nano: Int = 0 ) = {
    val r1 = java.time.LocalDateTime.of( year, month, day, hrs, min, sec, nano )
    // parsedDate.atStartOfDay(off).toInstant() // for java.time.Local
    r1.atZone( zoneID_UTC ).toInstant()
  }

}

