package pt.inescn.utils

/**
 *  Evaluating Real-time Anomaly Detection Algorithms - the Numenta Anomaly Benchmark
 * Alexander Lavin, Subutai Ahmad
 * @see https://numenta.com/numenta-anomaly-benchmark/
 * @see https://github.com/numenta/NAB
 * @see https://arxiv.org/abs/1510.03336
 *  
 * NAB Data Corpus
 * _____________
 *
 * 1. realAWSCloudwatch - hand labeled
 * 2. realAdExchange - hand labeled
 * 3. realKnownCause - no hand labeling
 *     1. ambient_temperature_system_failure.csv
 *     2. cpu_utilization_asg_misconfiguration.csv
 *     3. ec2_request_latency_system_failure.csv
 *     4. machine_temperature_system_failure.csv  <------
 *     5. nyc_taxi.csv
 *     6. rogue_agent_key_hold.csv
 *     7. rogue_agent_key_updown.csv
 *  4. realRogueAgent
 *  5. realTraffic
 *  6. realTweets
 *  7. artificialNoAnomaly
 *  8. artificialWithAnomaly
 *
 *  "No hand labeling" means that the contributed data already contains the labels were a failure occurred.
 *  The other data is hand-labeled following a pre-defined protocol (link missing in the NAB FAQ but
 *  can be found in Wiki Home). The labels are provided in JSON format and are found here:
 *  https://github.com/numenta/NAB/tree/master/labels
 *
 *  Twitter Anomaly Detector
 *  "NAB are CSV files with a "timestamp" column and a "value" column. The values are floats or integers, and the
 *  timestamps are strings of the form YYYY-mm-dd HH:MM:SS.s (in Python notation)."
 *
 * @see https://github.com/numenta/NAB/tree/master/data#nab-data-corpus
 * @see https://github.com/numenta/NAB/tree/master/data
 * @see https://github.com/numenta/NAB/wiki/Twitter-Anomaly-Detector
 * @see https://github.com/numenta/NAB/wiki#reporting-results-with-nabreporting-results-with-nab
 * @see https://drive.google.com/file/d/0B1_XUjaAXeV3YlgwRXdsb3Voa1k/view
 * 
 */
object NABUtils {

  val algorithm = "autoencoder"

  val data_dir = "/home/hmf/my_py2/download/NAB/data/"
  val label_dir = "/home/hmf/my_py2/download/NAB/labels"
  val results_dir = "/home/hmf/my_py2/download/NAB/results"

  val combined_labels = "combined_labels.json"
  val combined_windows = "combined_windows.json"
  val combined_windows_tiny = "combined_windows_tiny.json"

  //import scala.io.Source 
  import better.files._
  import java.io.{ File => JFile }

  /**
   * Get all files listed in the directory
   */
  def allFiles( dirName: String ): Option[ Seq[ File ] ] = {
    val dir = File( dirName )
    if ( !dir.isDirectory )
      None
    else {
      //val matches: Iterator[File] = dir.glob("**") 
      val matches: Iterator[ File ] = dir.glob( "**" )
      Some( matches.toList )
    }
  }

  /**
   *  Get all the data files
   */
  def allDataFiles( ext: String = "csv" ): Option[ Seq[ File ] ] =
    allFiles( data_dir ).map { _.filter { _.extension( includeDot = false ).exists { e => e.equals( ext ) } } }

  /**
   *  Get all the label files
   */
  def allLabelFiles( ext: String = "json" ): Option[ Seq[ File ] ] =
    allFiles( label_dir ).map { _.filter { _.extension( includeDot = false ).exists { e => e.equals( ext ) } } }

  /*
(root/"tmp"/"diary.txt")
  .createIfNotExists()  
  */

  import java.time.LocalDate
  //import org.threeten.extra.Interval
  import org.joda.time.Interval

  def runAlgo( data: File ) = ???
  def addDetection( data: File, detections: List[ Double ] ) = ???
  def addLabels( detections: List[ Interval ], windows: List[ Interval ] ) = ???

  def runExp( data: File, windows: Map[ String, List[ Interval ] ] ) = {
    val a = runAlgo( data )
    val d = addDetection( data, a )
    val r = addLabels( d, windows( data.nameWithoutExtension ) )
  }

  import org.json4s._
  //import org.json4s.jackson.JsonMethods._
  import org.json4s.native.JsonMethods._

  //implicit val formats = DefaultFormats // Brings in default date formats etc.
  implicit val formats = new DefaultFormats {
    import java.text.SimpleDateFormat
    // https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
    override def dateFormatter = new SimpleDateFormat( "yyyy-MM-dd HH:mm:ss.SSSSSS" )
  }

  // https://www.mkyong.com/java8/java-8-how-to-convert-string-to-localdate/
  // http://www.threeten.org/threeten-extra/apidocs/org/threeten/extra/Interval.html
  // https://www.playframework.com/documentation/2.4.x/ScalaJson
  // https://github.com/lift/framework/tree/master/core/json
  // http://spray.io/
  // https://github.com/fommil/spray-json-shapeless
  // http://json4s.org/
  // http://argonaut.io/
  // https://github.com/circe/circe
  // https://github.com/sphereio/sphere-scala-libs/tree/master/json (uses json4s)
  // https://github.com/non/jawn
  // https://github.com/propensive/rapture/blob/dev/doc/json.md
  // scala.util.parsing
  // https://github.com/scala/scala-parser-combinators
  // https://github.com/julienrf/play-json-derived-codecs
  // https://github.com/mandubian/play-json-zipper
  // https://github.com/rjmac/rojoma-json
  def readWindows( file: File ): Map[ String, List[ Interval ] ] = ???

  // https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
  // java.time
  // https://www.hackingnote.com/en/scala/datetime/
  // http://www.threeten.org/threeten-extra/
  //    http://www.threeten.org/threeten-extra/apidocs/org/threeten/extra/Interval.html
  // https://github.com/MenoData/Time4J
  // https://github.com/nscala-time/nscala-time
  // https://github.com/reactivecodes/scala-time
  // http://www.lamma.io/
  // https://github.com/maxcellent/lamma
  // 

  val list1 = List( Some( 1 ), None, Some( 2 ) )
  val list2 = list1.flatten // will be: List(1,2)
  val list3 = list1.flatMap { x => x } // will be: List(1,2)

  /**
   * This funtion reads
   */
  def windowToIntervals( windows: Map[ String, List[ List[ java.util.Date ] ] ] ) = {

    def makeInterval( t1: java.util.Date, t2: java.util.Date ): Option[ Interval ] = {
      val tn1 = t1.getTime
      val tn2 = t2.getTime
      if ( tn1 <= tn2 ) Some( new Interval( tn1, tn2 ) ) else None
    }

    def makeWindows( wins: List[ List[ java.util.Date ] ] ): Option[ List[ Option[ Interval ] ] ] = wins match {
      case Nil => None
      case _ =>
        Some( wins.map { win => if ( win.length != 2 ) None else makeInterval( win( 0 ), win( 1 ) ) } )
    }

    val t0 = windows.map { case ( k, wins ) => ( k, makeWindows( wins ) ) }
    val t1 = t0.collect { case ( k, Some( v ) ) => ( k, v.flatten ) }
    // val tx = t0.map { case (k, Some(wins)) => (k, wins.flatten  ) } // not complete
    t1
  }

}