package pt.inescn.utils
import org.scalatest._

// import scala.collection.JavaConverters._

/**
 *
 *
 *  test:compile
 *  test:console
 *  test:consoleQuick
 *  test:run
 *  test:runMain
 *
 * sbt test
 * sbt "testOnly pt.inescn.utils.NABUtilsSpec"
 *
 *
 */
class NABUtilsSpec extends FlatSpec with Matchers {
  import pt.inescn.utils.NABUtils._

  "Listing files" should "find the NAB data files" in {
    val dataFiles = allDataFiles()
    dataFiles shouldBe 'defined
    // May change in the future
    dataFiles.get should have size 58
    //println(  dataFiles.mkString( "," ) )
    //println(  dataFiles.size )
  }

  it should "find the NAB label files" in {
    val labelFiles = allLabelFiles()
    labelFiles shouldBe 'defined
    // May change in the future
    labelFiles.get should have size 10
    //println( labelFiles.mkString( "," ) )
    //println(  labelFiles.size )
  }

  import pt.inescn.utils.NABUtils._
  // JSON parsing
  import org.json4s._
  import org.json4s.native.JsonMethods._
  // Dates (JDK < 8)
  import java.util.Calendar
  import pt.inescn.utils.Utils._

  /**
   * Important note: if we use the hh:mm instead of HH:mm we get a
   * `Unable to obtain LocalTime from TemporalAccessor: {NanoOfSecond=0, MinuteOfHour=31,
   * MicroOfSecond=0, MilliOfSecond=0, HourOfAmPm=11, SecondOfMinute=13},ISO resolved to 2017-09-29 of
   * type java.time.format.Parsed`
   *
   * Problem is the parser does no know it is `am` or `pm` if this is  not indicated
   */
  "Basic date-time parsing" should "parse milliseconds" in {
    val datePattern = "yyyy-MM-dd HH:mm:ss.SSS"

    val str = "2017-09-29 11:31:13.123"
    val formatter = java.time.format.DateTimeFormatter.ofPattern( datePattern )
    val dateTime = java.time.LocalDateTime.parse( str, formatter )
    //println(dateTime)
    val r = java.time.LocalDateTime.of( 2017, 9, 29, 11, 31, 13, 123 * 1000000 )
    dateTime shouldBe r
  }

  it should "parse microseconds" in {
    val datePattern = "yyyy-MM-dd HH:mm:ss.SSSSSS"

    val str = "2017-09-29 11:31:13.123456"
    val formatter = java.time.format.DateTimeFormatter.ofPattern( datePattern )
    val dateTime = java.time.LocalDateTime.parse( str, formatter )
    //println(dateTime)
    val r = java.time.LocalDateTime.of( 2017, 9, 29, 11, 31, 13, 123456 * 1000 )
    dateTime shouldBe r
  }

  it should "parse nanoseconds" in {
    val datePattern = "yyyy-MM-dd HH:mm:ss.SSSSSSSSS"

    val str = "2017-09-29 11:31:13.123456789"
    val formatter = java.time.format.DateTimeFormatter.ofPattern( datePattern )
    val dateTime = java.time.LocalDateTime.parse( str, formatter )
    //println(dateTime)
    val r = java.time.LocalDateTime.of( 2017, 9, 29, 11, 31, 13, 123456789 )
    dateTime shouldBe r
  }

  it should "parse nanoseconds (2)" in {
    val datePattern = "yyyy-MM-dd HH:mm:ss.nnnnnnnnn"

    val str = "2017-09-29 11:31:13.123456789"
    val formatter = java.time.format.DateTimeFormatter.ofPattern( datePattern )
    val dateTime = java.time.LocalDateTime.parse( str, formatter )
    //println(dateTime)
    val r = java.time.LocalDateTime.of( 2017, 9, 29, 11, 31, 13, 123456789 )
    dateTime shouldBe r
  }

  case class Event( domain: String, filePath: String, timestamp: Long )

  /**
   * From: https://gist.github.com/djamelz/f963cab45933d05b2e10d8680e4213b6
   */
  "JSON Custom serializer" should "parse dates with microseconds" in {
    import org.joda.time.DateTime
    import org.joda.time.format.DateTimeFormat
    import org.json4s._
    import org.json4s.native.JsonMethods._

    val datePattern = "yyyy-MM-dd HH:mm:ss.SSSSSS"
    val s = """{"timestamp": "2016-09-29 11:31:13.247772", "domain": "d1", "filePath": "s3://..."}"""

    object StringToLong extends CustomSerializer[ Long ]( format => (
      { case JString( x ) => DateTime.parse( x, DateTimeFormat.forPattern( datePattern ) ).getMillis },
      { case x: Long => JInt( x ) } ) )

    implicit val formats = DefaultFormats + StringToLong

    val event = parse( s ).extract[ Event ]
    //println( event )
    event.timestamp shouldBe 1475145073247L
  }

  case class Event1( domain: String, filePath: String, timestamp: java.time.LocalDateTime )

  /**
   * From: https://gist.github.com/djamelz/f963cab45933d05b2e10d8680e4213b6
   */
  it should "parse dates with microseconds to JDK's java.time.LocalDateTime" in {
    import org.json4s._
    import org.json4s.native.JsonMethods._

    //val datePattern = "yyyy-MM-dd HH:mm:ss.SSSSSS"
    val s = """{"timestamp": "2016-09-29 11:31:13.247772", "domain": "d1", "filePath": "s3://..."}"""

    implicit val formats = DefaultFormats + StringToJDKLocalDateTime

    val event = parse( s ).extract[ Event1 ]
    //println( event )
    val r = java.time.LocalDateTime.of( 2016, 9, 29, 11, 31, 13, 247772 * 1000 )
    event.timestamp shouldBe r
  }

  case class Event2( domain: String, filePath: String, timestamp: java.time.Instant )
  
  it should "parse dates with microseconds to JDK's Instant" in {
    import org.json4s._
    import org.json4s.native.JsonMethods._

    //val datePattern = "yyyy-MM-dd HH:mm:ss.SSSSSS"
    val s = """{"timestamp": "2016-09-29 11:31:13.247772", "domain": "d1", "filePath": "s3://..."}"""

    implicit val formats = DefaultFormats + StringToJDKInstant

    val event = parse( s ).extract[ Event2 ]
    //println( event )
    val r = java.time.LocalDateTime.of( 2016, 9, 29, 11, 31, 13, 247772 * 1000 )
    val off = java.time.ZoneId.of("UTC")
    // parsedDate.atStartOfDay(off).toInstant() // for java.time.Local
    val r1 = r.atZone(off).toInstant()
    event.timestamp shouldBe r1
  }

  /*
  case class Event2( domain: String, filePath: String, timestamp: java.time.Instant )
  // https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
  // http://stackoverflow.com/questions/37672012/how-to-create-java-time-instant-from-pattern
  // http://stackoverflow.com/questions/33477695/why-does-the-new-java-8-date-time-api-not-have-nanosecond-precision
  // http://stackoverflow.com/questions/27454025/unable-to-obtain-localdatetime-from-temporalaccessor-when-parsing-localdatetime
  */  
  
  // Cannot be defined in method body - JSON4S barfs complaining about this. 
  case class OneDate( t1: java.time.Instant )

  "Parsing the JSON window file " should "parse the date with microsecond precision" in {

    val json0 = parse( """{
            "t1" : "2014-04-10 16:15:00.000001"
            }
      """ )
      
    implicit val formats = DefaultFormats + StringToJDKInstant
    val w0 = json0.extract[ OneDate ]
    //println( json0 )
    //println( w0 )
    json0.toString shouldBe "JObject(List((t1,JString(2014-04-10 16:15:00.000001))))"

    val r = java.time.LocalDateTime.of( 2014, 4, 10, 16, 15, 0, 1 * 1000 )
    val off = java.time.ZoneId.of("UTC")
    // parsedDate.atStartOfDay(off).toInstant() // for java.time.Local
    val r1 = r.atZone(off).toInstant()
    w0.t1.compareTo( r1 ) shouldBe 0 
  }

  it should "parse the date format with millisecond precision" in {

    val json0 = parse( """{
            "t1" : "2014-04-10 16:15:00.001000"
            }
      """ )
    implicit val formats = DefaultFormats + StringToJDKInstant
    val w0 = json0.extract[ OneDate ]
    //println( json0 )
    //println( w0 )
    json0.toString shouldBe "JObject(List((t1,JString(2014-04-10 16:15:00.001000))))"

    val r = java.time.LocalDateTime.of( 2014, 4, 10, 16, 15, 0, 1 * 1000000 )
    val off = java.time.ZoneId.of("UTC")
    // parsedDate.atStartOfDay(off).toInstant() // for java.time.Local
    val r1 = r.atZone(off).toInstant()
    w0.t1.compareTo( r1 ) shouldBe 0 
  }
  
  it should "parse a list of dates correctly" in {
    val json1 = parse( """
        [
            "2014-04-10 16:15:00.000002",
            "2014-04-12 01:45:00.000003"
        ]
      """ )
    implicit val formats = DefaultFormats + StringToJDKInstant
    //println( json1 )
    val w1 = json1.extract[ List[ java.time.Instant] ]
    //println( w1.mkString( "," ) )
    w1.size shouldBe 2
    val r1 = java.time.LocalDateTime.of( 2014, 4, 10, 16, 15, 0, 2 * 1000 )
    val r2 = java.time.LocalDateTime.of( 2014, 4, 12, 1, 45, 0, 3 * 1000 )
    val off = java.time.ZoneId.of("UTC")
    // parsedDate.atStartOfDay(off).toInstant() // for java.time.Local
    val ri1 = r1.atZone(off).toInstant()
    val ri2 = r2.atZone(off).toInstant()
    //println(ri1)
    //println(ri2)
    w1(0).compareTo( ri1 ) shouldBe 0 
    w1(1).compareTo( ri2 ) shouldBe 0 
  }

  it should "parse a list of list of dates correctly" in {
    val json2 = parse( """[
        [
            "2014-02-19 10:50:00.000000",
            "2014-02-20 03:30:00.000000"
        ],
        [
            "2014-02-23 11:45:00.000000",
            "2014-02-24 04:25:00.000000"
        ]
      ]
      """ )
    //println( json2 )
    implicit val formats = DefaultFormats + StringToJDKInstant
    val w2 = json2.extract[ List[ List[ java.time.Instant ] ] ]
    //println( w2.mkString( ";" ) )
    w2.size shouldBe 2

    w2( 0 ).size shouldBe 2
    val d1 = makeInstance( year = 2014, month = 2, day = 19, hrs = 10, min = 50, sec = 0, nano = 0 )
    val d2 = makeInstance( year = 2014, month = 2, day = 20, hrs = 3, min = 30, sec = 0, nano = 0 )
    w2( 0 )( 0 ).compareTo( d1 ) shouldBe 0
    w2( 0 )( 1 ).compareTo( d2 ) shouldBe 0

    w2( 1 ).size shouldBe 2
    val d3 = makeInstance( year = 2014, month = 2, day = 23, hrs = 11, min = 45, sec = 0, nano = 0 )
    val d4 = makeInstance( year = 2014, month = 2, day = 24, hrs = 4, min = 25, sec = 0, nano = 0 )
    w2( 1 )( 0 ).compareTo( d3 ) shouldBe 0
    w2( 1 )( 1 ).compareTo( d4 ) shouldBe 0
  }

 
  it should "map the data-sets windows to list of lists of dates correctly" in {
    val json3 = parse( """
            {
                "artificialNoAnomaly/art_daily_no_noise.csv": [],
                "artificialNoAnomaly/art_daily_perfect_square_wave.csv": [],
                "artificialNoAnomaly/art_daily_small_noise.csv": [],
                "artificialNoAnomaly/art_flatline.csv": [],
                "artificialNoAnomaly/art_noisy.csv": [],
                "artificialWithAnomaly/art_daily_flatmiddle.csv": [
                    [
                        "2014-04-10 07:15:00.000000",
                        "2014-04-11 16:45:00.000000"
                    ]
                ]
            }
            """ )
    //println( json3 )
    implicit val formats = DefaultFormats + StringToJDKInstant
    val w3 = json3.extract[ Map[ String, List[ List[ java.time.Instant ] ] ] ]
    println( w3.mkString( ";\n" ) )

    val d1 = w3( "artificialNoAnomaly/art_daily_no_noise.csv" )
    d1.size shouldBe 0
    val d2 = w3( "artificialNoAnomaly/art_daily_perfect_square_wave.csv" )
    d2.size shouldBe 0
    val d3 = w3( "artificialNoAnomaly/art_daily_small_noise.csv" )
    d3.size shouldBe 0
    val d4 = w3( "artificialNoAnomaly/art_flatline.csv" )
    d4.size shouldBe 0
    val d5 = w3( "artificialNoAnomaly/art_noisy.csv" )
    d5.size shouldBe 0
    val d6 = w3( "artificialWithAnomaly/art_daily_flatmiddle.csv" )
    d6.size shouldBe 1
    d6( 0 ).size shouldBe 2
    val dt1 = makeInstance( year = 2014, month = 4, day = 10, hrs = 7, min = 15, sec = 0, nano = 0 )
    val dt2 = makeInstance( year = 2014, month = 4, day = 11, hrs = 16, min = 45, sec = 0, nano = 0 )
    d6( 0 )( 0 ).compareTo( dt1 ) shouldBe 0
    d6( 0 )( 1 ).compareTo( dt2 ) shouldBe 0
  }

/*
  import java.time.LocalDateTime
  import java.time.format.DateTimeFormatter
  import java.time.ZoneId
  import java.time.ZonedDateTime
  import java.time.ZoneOffset
  import java.time.Instant
  import java.time.Clock
  */
  
  "Labelling the JSON Window file" should "generate the labels for a time-stamp list for a given data-set windows (exclusive)" in {
    val json3 = parse( """
            {
                "artificialNoAnomaly/art_daily_no_noise.csv": [],
                "artificialNoAnomaly/art_daily_perfect_square_wave.csv": [],
                "artificialNoAnomaly/art_daily_small_noise.csv": [],
                "artificialNoAnomaly/art_flatline.csv": [],
                "artificialNoAnomaly/art_noisy.csv": [],
                "artificialWithAnomaly/art_daily_flatmiddle.csv": [
                    [
                        "2014-04-10 07:15:00.000010",
                        "2014-04-10 07:15:00.000020",
                    ]
                ]
            }
            """ )
    //println( json3 )
    implicit val formats = DefaultFormats + StringToJDKInstant
    val w3 = json3.extract[ Map[ String, List[ List[ java.time.Instant ] ] ] ]
    //println( w3.mkString( ";\n" ) )

    val w4 = windowToIntervals( w3 )
    println( w4 )
    
    val delta = 5 * 1000 // 5 us 
    val start = makeInstance(2014, 4, 10, 7, 15, 0, 0)
    val dts = 0 to (10 * delta ) by delta  map { n => start.plusNanos(n) } 
    val dates = dts.toList
    println( dates.mkString(", ++.. \n") )
        
    val labels = labelInstanceExclusive( dates, w4("artificialWithAnomaly/art_daily_flatmiddle.csv") )
    println( labels.mkString(",\n") )
    
    labels should have size 11
    labels should be (List(0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)) 
  }
  
  
  "Labelling the JSON Window file" should "generate the labels for a time-stamp list for a given data-set windows (inclusive)" in {
    val json3 = parse( """
            {
                "artificialNoAnomaly/art_daily_no_noise.csv": [],
                "artificialNoAnomaly/art_daily_perfect_square_wave.csv": [],
                "artificialNoAnomaly/art_daily_small_noise.csv": [],
                "artificialNoAnomaly/art_flatline.csv": [],
                "artificialNoAnomaly/art_noisy.csv": [],
                "artificialWithAnomaly/art_daily_flatmiddle.csv": [
                    [
                        "2014-04-10 07:15:00.000010",
                        "2014-04-10 07:15:00.000020",
                    ]
                ]
            }
            """ )
    //println( json3 )
    implicit val formats = DefaultFormats + StringToJDKInstant
    val w3 = json3.extract[ Map[ String, List[ List[ java.time.Instant ] ] ] ]
    //println( w3.mkString( ";\n" ) )

    val w4 = windowToIntervals( w3 )
    println( w4 )
    
    val delta = 5 * 1000 // 5 us 
    val start = makeInstance(2014, 4, 10, 7, 15, 0, 0)
    val dts = 0 to (10 * delta ) by delta  map { n => start.plusNanos(n) } 
    val dates = dts.toList
    println( dates.mkString(", ++.. \n") )
        
    val labels = labelInstanceInclusive( dates, w4("artificialWithAnomaly/art_daily_flatmiddle.csv") )
    println( labels.mkString(",\n") )
    
    labels should have size 11
    labels should be (List(0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)) 
  }
  
  // TODO: check if we label correctly when next t is 2 windows ahead
  // TODO: check addLabels to a table 
  
}