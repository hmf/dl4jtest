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

  // Cannot be defined n method body - JSON4S barfs complaining about this. 
  case class OneDate( t1: java.util.Date )

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

  "Parsing the JSON window file " should "parse the date format correctly" in {

    val json0 = parse( """{
            "t1" : "2014-04-10 16:15:00.000000"
            }
      """ )
    val w0 = json0.extract[ OneDate ]
    //println( json0 )
    //println( w0 )
    json0.toString shouldBe "JObject(List((t1,JString(2014-04-10 16:15:00.000000))))"

    val utilDate = makeData( year = 2014, month = 4, date = 10, hrs = 16, min = 15, sec = 0, milli = 0 ) // cal.getTime()  
    val t = OneDate( utilDate )
    //println(w0.t1)
    //println(utilDate)
    //println(w0.t1.compareTo(utilDate))
    //w0 shouldBe t
    //w0 should equal (t)
    //w0 should === (t)
    //w0 shouldBe  OneDate( new java.util.Date(year, month, date, hrs, min, sec) )
    //w0 shouldEqual(t)
    w0.t1.compareTo( utilDate ) shouldBe 0
  }

  it should "parse a list of dates correctly" in {
    val json1 = parse( """
        [
            "2014-04-10 16:15:00.000000",
            "2014-04-12 01:45:00.000000"
        ]
      """ )
    //println( json1 )
    val w1 = json1.extract[ List[ java.util.Date ] ]
    //println( w1.mkString( "," ) )
    w1.size shouldBe 2
    val d1 = makeData( year = 2014, month = 4, date = 10, hrs = 16, min = 15, sec = 0, milli = 0 )
    val d2 = makeData( year = 2014, month = 4, date = 12, hrs = 1, min = 45, sec = 0, milli = 0 )
    w1( 0 ).compareTo( d1 ) shouldBe 0
    w1( 1 ).compareTo( d2 ) shouldBe 0
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
    val w2 = json2.extract[ List[ List[ java.util.Date ] ] ]
    //println( w2.mkString( ";" ) )
    w2.size shouldBe 2
    
    w2( 0 ).size shouldBe 2
    val d1 = makeData( year = 2014, month = 2, date = 19, hrs = 10, min = 50, sec = 0, milli = 0 )
    val d2 = makeData( year = 2014, month = 2, date = 20, hrs = 3, min = 30, sec = 0, milli = 0 )
    w2( 0 )( 0 ).compareTo( d1 ) shouldBe 0
    w2( 0 )( 1 ).compareTo( d2 ) shouldBe 0
    
    w2( 1 ).size shouldBe 2
    val d3 = makeData( year = 2014, month = 2, date = 23, hrs = 11, min = 45, sec = 0, milli = 0 )
    val d4 = makeData( year = 2014, month = 2, date = 24, hrs = 4, min = 25, sec = 0, milli = 0 )
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
    val w3 = json3.extract[ Map[ String, List[ List[ java.util.Date ] ] ] ]
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
    d6(0).size shouldBe 2
    val dt1 = makeData( year = 2014, month = 4, date = 10, hrs = 7, min = 15, sec = 0, milli = 0 )
    val dt2 = makeData( year = 2014, month = 4, date = 11, hrs = 16, min = 45, sec = 0, milli = 0 )
    d6( 0 )( 0 ).compareTo( dt1 ) shouldBe 0
    d6( 0 )( 1 ).compareTo( dt2 ) shouldBe 0
    
  }
}