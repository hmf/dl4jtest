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

  "Parsing the JSON data file " should "parse the date format correctly" in {
    
    import pt.inescn.utils.NABUtils._
    import org.json4s._
    import org.json4s.native.JsonMethods._

    val json0 = parse( """{
            "t1" : "2014-04-10 16:15:00.000000"
            }
      """ )
    val w0 = json0.extract[ OneDate ]
    println( json0 )
    println( w0 )
    json0.toString shouldBe "JObject(List((t1,JString(2014-04-10 16:15:00.000000))))"
    
    import java.util.Calendar 
    
    val year = 2014
    val month = 4
    val date = 10 
    val hrs = 16
    val min = 15 
    val sec = 0
    
    val cal = Calendar.getInstance()
    cal.clear()
    cal.set(Calendar.YEAR, year)
    cal.set(Calendar.MONTH, month-1)
    cal.set(Calendar.DATE, date)
    cal.set(Calendar.HOUR, hrs)
    cal.set(Calendar.MINUTE, min)
    cal.set(Calendar.SECOND, sec)
    cal.set(Calendar.MILLISECOND, 0)

    val utilDate = cal.getTime();    
    val t = OneDate( utilDate )
    println(w0.t1)
    println(utilDate)
    println(w0.t1.compareTo(utilDate))
    //w0 shouldBe t
    //w0 should equal (t)
    //w0 should === (t)
    //w0 shouldBe  OneDate( new java.util.Date(year, month, date, hrs, min, sec) )
    w0 shouldEqual(t)
    
  }

  it should "do another B" in {
  }

}