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
    val off = java.time.ZoneId.of( "UTC" )
    // parsedDate.atStartOfDay(off).toInstant() // for java.time.Local
    val r1 = r.atZone( off ).toInstant()
    event.timestamp shouldBe r1
  }

  /*
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
    val off = java.time.ZoneId.of( "UTC" )
    // parsedDate.atStartOfDay(off).toInstant() // for java.time.Local
    val r1 = r.atZone( off ).toInstant()
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
    val off = java.time.ZoneId.of( "UTC" )
    // parsedDate.atStartOfDay(off).toInstant() // for java.time.Local
    val r1 = r.atZone( off ).toInstant()
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
    val w1 = json1.extract[ List[ java.time.Instant ] ]
    //println( w1.mkString( "," ) )
    w1.size shouldBe 2
    val r1 = java.time.LocalDateTime.of( 2014, 4, 10, 16, 15, 0, 2 * 1000 )
    val r2 = java.time.LocalDateTime.of( 2014, 4, 12, 1, 45, 0, 3 * 1000 )
    val off = java.time.ZoneId.of( "UTC" )
    // parsedDate.atStartOfDay(off).toInstant() // for java.time.Local
    val ri1 = r1.atZone( off ).toInstant()
    val ri2 = r2.atZone( off ).toInstant()
    //println(ri1)
    //println(ri2)
    w1( 0 ).compareTo( ri1 ) shouldBe 0
    w1( 1 ).compareTo( ri2 ) shouldBe 0
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
    //println( w3.mkString( ";\n" ) )

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
    //println( w4 )

    val delta = 5 * 1000 // 5 us 
    val start = makeInstance( 2014, 4, 10, 7, 15, 0, 0 )
    val dts = 0 to ( 10 * delta ) by delta map { n => start.plusNanos( n ) }
    val dates = dts.toList
    //println( dates.mkString(", ++.. \n") )

    val labels = labelInstanceExclusive( dates, w4( "artificialWithAnomaly/art_daily_flatmiddle.csv" ) )
    //println( labels.mkString(",\n") )

    labels should have size 11
    labels should be ( List( 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 ) )
  }

  it should "generate the labels for a time-stamp list for a given data-set windows (inclusive)" in {
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
    //println( w4 )

    val delta = 5 * 1000 // 5 us 
    val start = makeInstance( 2014, 4, 10, 7, 15, 0, 0 )
    val dts = 0 to ( 10 * delta ) by delta map { n => start.plusNanos( n ) }
    val dates = dts.toList
    //println( dates.mkString(", ++.. \n") )

    val labels = labelInstanceInclusive( dates, w4( "artificialWithAnomaly/art_daily_flatmiddle.csv" ) )
    //println( labels.mkString(",\n") )

    labels should have size 11
    labels should be ( List( 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0 ) )
  }

  it should "generate the labels for a time-stamp list for multiple windows (inclusive)" in {
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
                    [
                        "2014-04-10 07:15:00.000030",
                        "2014-04-10 07:15:00.000040",
                    ]
                ]
            }
            """ )
    //println( json3 )
    implicit val formats = DefaultFormats + StringToJDKInstant
    val w3 = json3.extract[ Map[ String, List[ List[ java.time.Instant ] ] ] ]
    //println( w3.mkString( ";\n" ) )

    val w4 = windowToIntervals( w3 )
    //println( w4 )

    val delta = 5 * 1000 // 5 us 
    val start = makeInstance( 2014, 4, 10, 7, 15, 0, 0 )
    val dts = 0 to ( 10 * delta ) by delta map { n => start.plusNanos( n ) }
    val dates = dts.toList
    //println( dates.mkString(", ++.. \n") )

    val labels = labelInstanceInclusive( dates, w4( "artificialWithAnomaly/art_daily_flatmiddle.csv" ) )
    //println( labels.mkString(",\n") )

    labels should have size 11
    labels should be ( List( 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0 ) )
  }

  def parseInstantUTC( date: String ) = {
    import java.time.Instant
    import kantan.csv._
    import kantan.csv.ops._
    import kantan.csv.java8._
    // Import the NAB data file parser implicitly
    import pt.inescn.utils.NABUtils._
    val instant = date.unsafeReadCsv[ List, Instant ]( rfc )
    instant( 0 )
  }

  "Labelling the DataFrame" should "load and parse the NAB data file correctly" in {
    import better.files._
    import better.files.Cmds._

    import java.time.Instant
    import kantan.csv._
    import kantan.csv.ops._
    import kantan.csv.java8._

    // Import the NAB data file parser implicitly
    import pt.inescn.utils.NABUtils._
    val input = "2014-04-14 23:55:00"
    val res = input.unsafeReadCsv[ List, Instant ]( rfc )
    //println(res.mkString(","))

    // We need to bring in shapeless "compile time reflection"
    // https://nrinaudo.github.io/kantan.csv/tut/shapeless.html
    import kantan.csv._
    import kantan.csv.ops._
    import kantan.csv.generic._

    // Data file to process
    val fileName = "art_increase_spike_density.csv"
    //println( pwd )
    val data = cwd / "data/nab/data/artificialWithAnomaly" / fileName // cwd = pwd
    val r = loadData( data )
    //println(r)
    r.isRight should be ( true )

    val d0 = r.getOrElse( NABUtils.NABFrame( List[ java.time.Instant ](), List[ Double ]() ) )
    d0.dt( 0 ) should be ( parseInstantUTC( "2014-04-01 00:00:00" ) )
    d0.value( 0 ) should be ( 20.0 )

    d0.dt( 1 ) should be ( parseInstantUTC( "2014-04-01 00:05:00" ) )
    d0.value( 1 ) should be ( 0.0 )

    d0.dt( 108 ) should be ( parseInstantUTC( "2014-04-01 09:00:00" ) )
    d0.value( 108 ) should be ( 0.0 )

    d0.dt( 4031 ) should be ( parseInstantUTC( "2014-04-14 23:55:00" ) )
    d0.value( 4031 ) should be ( 0.0 )
  }

  it should "should be able to load the result file (for testing)" in {
    import better.files._
    import better.files.Cmds._

    import NABUtils._
    import NABUtils.NABDataRow._

    // We need to bring in shapeless "compile time reflection"
    // https://nrinaudo.github.io/kantan.csv/tut/shapeless.html
    import kantan.csv._
    import kantan.csv.ops._
    import kantan.csv.generic._

    val dataDirectory = "data/nab/results/numentaTM/artificialWithAnomaly"
    val dataFileName = "art_increase_spike_density.csv"
    val algorithm_name = "numentaTM"

    val labelledp = cwd / dataDirectory / ( "numentaTM" + "_" + dataFileName )
    //val expected = loadResults( labelledp )
    val expected: Either[ List[ Throwable ], NABResultAll ] = loadResults( labelledp )
    //println(expected)
    expected.isRight should be ( true )
    val d0 = expected.getOrElse( emptyNABResultAll )
    val line0 = 0
    //println( d0.dt( line0) )   
    d0.dt( line0 ) should be ( parseInstantUTC( "2014-04-01 00:00:00" ) )
    //d0.value( line0 ) should be (20.0 +- 0.0)
    d0.value( line0 ) should be ( 20.0 )
    d0.anomaly_score( line0 ) should be ( 0.0301029996659 )
    d0.raw_score( line0 ) should be ( 1.0 )
    d0.label( line0 ) should be ( 0 )
    d0.reward_low_FP_rate( line0 ) should be ( 0.0 )
    d0.reward_low_FN_rate( line0 ) should be ( 0.0 )

    val line1 = 1422
    d0.dt( line1 ) should be ( parseInstantUTC( "2014-04-05 22:30:00" ) )
    d0.value( line1 ) should be ( 0.0 )
    d0.anomaly_score( line1 ) should be ( 0.0034982625592 )
    d0.raw_score( line1 ) should be ( 0.0250000003725 )
    d0.label( line1 ) should be ( 0 )
    d0.reward_low_FP_rate( line1 ) should be ( 0.0 )
    d0.reward_low_FN_rate( line1 ) should be ( 0.0 )

    val line2 = 4031
    d0.dt( line2 ) should be ( parseInstantUTC( "2014-04-14 23:55:00" ) )
    d0.value( line2 ) should be ( 0.0 )
    d0.anomaly_score( line2 ) should be ( 0.0115668894529 )
    d0.raw_score( line2 ) should be ( 0.0 )
    d0.label( line2 ) should be ( 0 )
    d0.reward_low_FP_rate( line2 ) should be ( 0.0 )
    d0.reward_low_FN_rate( line2 ) should be ( 0.0 )

  }

  it should "should be able to load the failure labels (windows)" in {
    import better.files._
    import better.files.Cmds._

    import NABUtils._
    import NABUtils.NABDataRow._

    // We need to bring in shapeless "compile time reflection"
    // https://nrinaudo.github.io/kantan.csv/tut/shapeless.html
    import kantan.csv._
    import kantan.csv.ops._
    import kantan.csv.generic._

    val labelsDirectory = "data/nab/labels"
    val labelsFileName = "combined_windows.json"
    val algorithm_name = "numentaTM"

    val labels = cwd / labelsDirectory / labelsFileName
    val wins = loadJSONLabels( labels )
    //println(wins.size)
    wins.size should be ( 58 )

    val file1 = "artificialNoAnomaly/art_daily_no_noise.csv"
    wins.contains( file1 ) should be ( true )
    val empty1 = wins( file1 )
    empty1.length should be ( 0 )

    val file2 = "artificialNoAnomaly/art_daily_perfect_square_wave.csv"
    wins.contains( file2 ) should be ( true )
    val empty2 = wins( file2 )
    empty2.length should be ( 0 )

    val file3 = "artificialNoAnomaly/art_daily_small_noise.csv"
    wins.contains( file3 ) should be ( true )
    val empty3 = wins( file3 )
    empty3.length should be ( 0 )

    val file4 = "artificialNoAnomaly/art_flatline.csv"
    wins.contains( file4 ) should be ( true )
    val empty4 = wins( file4 )
    empty4.length should be ( 0 )

    val file5 = "artificialNoAnomaly/art_noisy.csv"
    wins.contains( file5 ) should be ( true )
    val empty5 = wins( file5 )
    empty5.length should be ( 0 )

    val file6 = "artificialWithAnomaly/art_daily_flatmiddle.csv"
    wins.contains( file6 ) should be ( true )
    val wins6 = wins( file6 )
    wins6.length should be ( 1 )
    val i6 = makeInterval( parseInstantUTC( "2014-04-10 07:15:00.000000" ), parseInstantUTC( "2014-04-11 16:45:00.000000" ) )
    wins6( 0 ) should be ( i6.get )

    val file7 = "realAWSCloudwatch/ec2_network_in_5abac7.csv"
    wins.contains( file7 ) should be ( true )
    val wins7 = wins( file7 )
    wins7.length should be ( 2 )
    val i71 = makeInterval( parseInstantUTC( "2014-03-10 09:06:00.000000" ), parseInstantUTC( "2014-03-11 04:46:00.000000" ) )
    wins7( 0 ) should be ( i71.get )
    val i72 = makeInterval( parseInstantUTC( "2014-03-12 11:11:00.000000" ), parseInstantUTC( "2014-03-13 06:51:00.000000" ) )
    wins7( 1 ) should be ( i72.get )

    val file8 = "realTweets/Twitter_volume_UPS.csv"
    wins.contains( file8 ) should be ( true )
    val wins8 = wins( file8 )
    wins8.length should be ( 5 )
    val i81 = makeInterval( parseInstantUTC( "2015-03-02 11:17:53.000000" ), parseInstantUTC( "2015-03-03 13:37:53.000000" ) )
    wins8( 0 ) should be ( i81.get )
    val i85 = makeInterval( parseInstantUTC( "2015-03-29 03:17:53.000000" ), parseInstantUTC( "2015-03-30 05:37:53.000000" ) )
    wins8( 4 ) should be ( i85.get )

  }

  it should "generate the labels for a time-stamp list for a given data-set windows (inclusive)" in {
    import better.files._
    import better.files.Cmds._

    import NABUtils._
    import NABUtils.NABDataRow._

    // Files to process
    val datasetId = "artificialWithAnomaly"
    val dataFileName = "art_increase_spike_density.csv"
    val dataset = datasetId + "/" + dataFileName
    val labelsFileName = "combined_windows.json"

    val labelsp = cwd / "data/nab/labels" / labelsFileName
    val labels = loadJSONLabels( labelsp )
    //println(labels)
    //println(labels( dataset ))
    labels.isEmpty should be ( false )

    // We need to bring in shapeless "compile time reflection"
    // https://nrinaudo.github.io/kantan.csv/tut/shapeless.html
    import kantan.csv._
    import kantan.csv.ops._
    import kantan.csv.generic._

    // This is the original data
    val datap = cwd / "data/nab/data" / datasetId / dataFileName
    val data = loadData( datap )
    //println(data)
    data.isRight should be ( true )

    // This is the result data that has already been labeled and scored
    val labelledp = cwd / "data/nab/results/numentaTM" / datasetId / ( "numentaTM_" + dataFileName )
    //val expected = loadResults( labelledp )
    val expected: Either[ List[ Throwable ], NABResultAll ] = loadResults( labelledp )
    //println(expected)
    expected.isRight should be ( true )

    // We now label the original data
    val wins = labels.getOrElse( dataset, List[ org.threeten.extra.Interval ]() )
    //println(data.getOrElse(emptyNABFrame).dt)
    //println(wins.mkString("\n"))
    val new_labels = addLabels( labelInstanceInclusive )( data.getOrElse( emptyNABFrame ), wins )
    // And check that it is the same as the result data labels
    val old_labels = expected.getOrElse( emptyNABResultAll )
    new_labels.dt should contain theSameElementsInOrderAs old_labels.dt
    new_labels.dt should be ( sorted )
    new_labels.label should contain theSameElementsInOrderAs old_labels.label
  }

  it should "should add the detection correctly (assumes a perfect detector here for easy checking)" in {

    val ts1 = parseInstantUTC( "2014-04-05 22:30:00" )
    val bytes1 = toBytes( ts1 )
    val hex1 = Hex.valueOf( bytes1 )
    //println( bytes1.size )
    //println( hex1 )
    //println( hex1.size )
    bytes1.size shouldBe (20) 
    hex1.size shouldBe (2*bytes1.size) 
    hex1 shouldBe ("323031342D30342D30355432323A33303A30305A") 

    import java.security.MessageDigest
    val digest1 = MessageDigest.getInstance( "SHA-256" )
    updateHash( ts1 )( digest1 )
    val hash1 = digest1.digest
    val hexh1 = Hex.valueOf( hash1 )
    //println(hex2)
    // 256 bits -> / 8 bytes -> * 2 Hex digits (4 bits)
    hexh1.size shouldBe ((256 / 8) * 2 )
    
    val bytes2 = toBytes( 20.00001)
    val hex2 = Hex.valueOf( bytes2 )
    //println( bytes2.size )
    //println( hex2 )
    //println( hex2.size )
    bytes2.size shouldBe ( 8 ) 
    hex2.size shouldBe (2*bytes2.size) 

    val digest2 = MessageDigest.getInstance( "SHA-256" )
    updateHash( 20.00001 )( digest2 )
    val hash2 = digest2.digest
    val hexh2 = Hex.valueOf( hash2 )
    //println(hexh2)
    // 256 bits -> / 8 bytes -> * 2 Hex digits (4 bits)
    hexh2.size shouldBe ((256 / 8) * 2 )


    val digest3 = MessageDigest.getInstance( "SHA-256" )
    updateHash( 20.000000000000001 )( digest3 )
    val hash3 = digest3.digest
    val hexh3 = Hex.valueOf( hash3 )
    //println(hexh3)
    // 256 bits -> / 8 bytes -> * 2 Hex digits (4 bits)
    val hashlen_256 = (256 / 8) * 2
    hexh3.size shouldBe ( hashlen_256 )
    hexh2 should not equal (hexh3)
    
    import better.files._
    import better.files.Cmds._

    import NABUtils._
    import NABUtils.NABDataRow._

    // Files to process
    val datasetId = "artificialWithAnomaly"
    val dataFileName1 = "art_increase_spike_density.csv"
    val dataset1 = datasetId + "/" + dataFileName1
    val dataFileName2 = "art_daily_jumpsdown.csv"
    val dataset2 = datasetId + "/" + dataFileName1

    // This is the result data that has already been labeled and scored
    val labelledp1 = cwd / "data/nab/results/numentaTM" / datasetId / ( "numentaTM_" + dataFileName1 )
    val labelledp2 = cwd / "data/nab/results/numentaTM" / datasetId / ( "numentaTM_" + dataFileName2 )

    // We need to bring in shapeless "compile time reflection"
    // https://nrinaudo.github.io/kantan.csv/tut/shapeless.html
    import kantan.csv._
    import kantan.csv.ops._
    import kantan.csv.generic._

    //import java.security.MessageDigest
    implicit val digest = MessageDigest.getInstance( "SHA-256" )
    val hashf1e : Either[List[Throwable], Array[Byte]] = tagData(labelledp1, sample_size = 0.15 )
    hashf1e.isRight should be ( true )
    val hashf1 = hashf1e.right.get
    println( Hex.valueOf( hashf1 ) )
    val hexf1 = Hex.valueOf( hashf1 )
    hexf1.size shouldBe ( hashlen_256 )
    
    digest.reset
    val hashf2e : Either[List[Throwable], Array[Byte]] = tagData( labelledp2, sample_size = 0.15 )
    println(hashf2e)
    hashf2e.isRight should be ( true )
    val hashf2 = hashf2e.right.get
    println( Hex.valueOf( hashf2 ) )
    val hexf2 = Hex.valueOf( hashf2 )
    hexf2.size shouldBe ( hashlen_256 )

  }

}