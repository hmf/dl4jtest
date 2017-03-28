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
 *
 * Files I/O
 * @see https://github.com/pathikrit/better-files
 *
 * Time
 * @see http://joda-time.sourceforge.net
 *
 * JSON
 * @see https://github.com/json4s/json4s
 * @see http://blog.takipi.com/the-ultimate-json-library-json-simple-vs-gson-vs-jackson-vs-json/
 * @see https://github.com/google/gson
 * @see https://github.com/FasterXML/jackson
 * @see https://jsonp.java.net/
 */
object NABUtils {

  val algorithm = "autoencoder"

  val data_dir = "/home/hmf/my_py2/download/NAB/data/"
  val label_dir = "/home/hmf/my_py2/download/NAB/labels"
  val results_dir = "/home/hmf/my_py2/download/NAB/results"

  val combined_labels = "combined_labels.json"
  val combined_windows = "combined_windows.json"
  val combined_windows_tiny = "combined_windows_tiny.json"

  val TIMESTAMP_H = "timestamp"
  val VALUE_H = "value"
  val ANOMALY_SCORE_H = "anomaly_score"
  val LABEL_H = "label"
  
  
  //import scala.io.Source 
  import better.files._
  import java.io.{ File => JFile }
  
  import collection.JavaConverters._

        
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
  import org.threeten.extra.Interval
  //import org.joda.time.Interval

  def runAlgo( data: File ) = ???
  def addDetection( data: File, detections: List[ Double ] ) = ???
  def addLabels( detections: List[ Interval ], windows: List[ Interval ] ) = ???

  def runExp( data: File, windows: Map[ String, List[ Interval ] ] ) = {
    val a = runAlgo( data )
    val d = addDetection( data, a )
    val r = addLabels( d, windows( data.nameWithoutExtension ) )
  }

  import org.json4s._
  import org.json4s.jackson.JsonMethods._
  // import org.json4s.native.JsonMethods._

  // http://stackoverflow.com/questions/27408180/json4s-conversion-to-java-sql-timestamp-does-not-work
  // https://gist.github.com/djamelz/f963cab45933d05b2e10d8680e4213b6

  //val dtFormatter = "yyyy-MM-dd HH:mm:ss.SSSSSS"
  val dtFormatter = "yyyy-MM-dd HH:mm:ss.SSS"

  //implicit val formats = DefaultFormats // Brings in default date formats etc.

  // Create your own formatter that overrised's JSON4s formatters
  // Note that this only works for java.util.Date that only has millisecond precision
  // see StringToJDKLocalDateTime for a way to circumvent this
  /*
  implicit val formats = new DefaultFormats {
    import java.text.SimpleDateFormat
    // https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
    override def dateFormatter = {
      val f = new SimpleDateFormat( dtFormatter )
      f.setTimeZone( DefaultFormats.UTC )
      f
    }
  }*/

  val datePattern = "yyyy-MM-dd HH:mm:ss.SSSSSS"
  val NABformatter = java.time.format.DateTimeFormatter.ofPattern( datePattern )

  object StringToJDKLocalDateTime extends CustomSerializer[ java.time.LocalDateTime ]( format => (
    {
      case JString( x ) =>
        java.time.LocalDateTime.parse( x, NABformatter )
    },
    {
      case x: java.time.LocalDateTime =>
        JString( x.format( NABformatter ) )
    } ) )

  // To use the above formatter you must add it implicitly to the context
  // implicit val formats = DefaultFormats + StringToJDKLocalDateTime

  import pt.inescn.utils.Utils.zoneID_UTC

  object StringToJDKInstant extends CustomSerializer[ java.time.Instant ]( format => (
    {
      case JString( x ) =>
        val dt = java.time.LocalDateTime.parse( x, NABformatter )
        // parsedDate.atStartOfDay(off).toInstant() // for java.time.Local
        dt.atZone( zoneID_UTC ).toInstant()
    },
    {
      case x: java.time.Instant =>
        val s = NABformatter.format( x )
        JString( s )
    } ) )

  // To use the above formatter you must add it implicitly to the context
  // implicit val formats = DefaultFormats + StringToJDKInstant

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

  /**
   * Converts two dates into an Interval. We use an interval that has nanosecond precision
   *
   * @see org.threeten.extra.Interval
   */
  def makeInterval( t1: java.time.Instant, t2: java.time.Instant ): Option[ Interval ] = {
    val i = Interval.of( t1, t2 )
    if ( t1.isBefore( t2 ) ) Some( i ) else None
  }

  /**
   * Takes a list of lists of instances and converts each sublist of instances into an `Interval`.
   * We assume that each sublist has only 2 elements: the start and end time `Instant` that
   * are used to create the `Interval`. If it does not contain 2 elements, a `None` is generated.
   * If the file's window list is empty (no list of pairs of time `Instant`) then a `None` is returned.
   *
   * @see makeListWindows
   */
  def makeOptionWindows( wins: List[ List[ java.time.Instant ] ] ): Option[ List[ Option[ Interval ] ] ] =
    wins match {
      case Nil => None
      case _ =>
        Some( wins.map { win => if ( win.length != 2 ) None else makeInterval( win( 0 ), win( 1 ) ) } )
    }

  /**
   * Takes a list of lists of instances and converts each sublist of instances into an `Interval`.
   * We assume that each sublist has only 2 elements: the start and end time `Instant` that
   * are used to create the `Interval`. If it does not contain 2 elements, a `None` is generated.
   * If the file's window list is empty then an empty list is also returned.
   *
   * @see  makeOptionWindows
   */
  def makeListWindows( wins: List[ List[ java.time.Instant ] ] ): List[ Option[ Interval ] ] =
    wins match {
      case Nil => Nil
      case _ =>
        wins.map { win => if ( win.length != 2 ) None else makeInterval( win( 0 ), win( 1 ) ) }
    }

  /**
   * This function takes in a map keyed by file name that contains a list of lists. We assume each sublist
   * contains only two elements: the start and end time-stamps of a anomaly window. Each pair is then
   * converted to an `Interval`. All `None`are filtered out and only the existing content of the `Some`
   * are returned.
   *
   * @see windowToIntervals
   */
  def windowToOptionIntervals( windows: Map[ String, List[ List[ java.time.Instant ] ] ] ) = {

    val t0 = windows.map { case ( k, wins ) => ( k, makeOptionWindows( wins ) ) }
    val t1 = t0.collect { case ( k, Some( v ) ) => ( k, v.flatten ) }
    // val tx = t0.map { case (k, Some(wins)) => (k, wins.flatten  ) } // not complete
    t1
  }

  /**
   * This function takes in a map keyed by file name that contains a list of lists. We assume each sublist
   * contains only two elements: the start and end time-stamps of a anomaly window. Each pair is then
   * converted to an `Interval`. Any data files with no anomaly window labels are returned as empty lists
   * (we still have to generate labels for these)
   *
   * @see windowToOptionIntervals
   */
  def windowToIntervals( windows: Map[ String, List[ List[ java.time.Instant ] ] ] ) = {

    val t0 = windows.map { case ( k, wins ) => ( k, makeListWindows( wins ) ) }
    val t1 = t0.collect { case ( k, v ) => ( k, v.flatten ) }
    t1
  }

  import com.github.lwhite1.tablesaw.api.Table
  /*import com.github.lwhite1.tablesaw.api.ColumnType
  import com.github.lwhite1.tablesaw.columns.Column
  import com.github.lwhite1.tablesaw.api.IntColumn
  import com.github.lwhite1.tablesaw.api.BooleanColumn
  import com.github.lwhite1.tablesaw.api.FloatColumn
  import com.github.lwhite1.tablesaw.api.CategoryColumn*/

  /**
   * Checks if `Instant` `d` is within the `Interval` `i`.
   * Test is inclusive with the start and exclusive with the end.
   */
  def checkExclusiveIn( i: Interval, d: java.time.Instant ) = i.contains( d )
  /**
   * Checks if `Instant` `d` is within the `Interval` `i`.
   * Test is inclusive both with the start and the end.
   */
  def checkInclusiveIn( i: Interval, d: java.time.Instant ) = i.contains( d ) || ( i.getEnd.compareTo( d ) == 0 )

  type Label = Int

  /**
   * This function checks if the `Instant` `d` is within any of the pending anomaly windows and if so labels
   * it as 1 otherwise it is 0. If the `Instant` `d` occurs before the next pending anomaly window, then
   * it is labeled as not an anomaly (0). If the `Instant` `d` is within an window (`Interval`), it is labeled
   * as an anomaly (1). The current window is not removed from the pending windows list because other instances
   * may also occur in the current `Interval`. If however the current `Instance` occurs after the current
   * window, we remove it from the pending windows and search the available pending `Interval`.
   *
   * Note that the function is parameterized with the interval checking function so that we can check for
   * either with an inclusive or exclusive end time. NAB uses an inclusive check.
   *
   * IMPORTANT: this algorithm assumes that the Interval windows are ordered chronologically.
   *
   * @see checkExclusiveIn
   * @see checkInclusiveIn
   */
  def isInInterval( chk: ( Interval, java.time.Instant ) => Boolean )( d: java.time.Instant, i: List[ Interval ] ): ( Label, List[ Interval ] ) = {
    @annotation.tailrec
    def inInterval( d: java.time.Instant, i: List[ Interval ] ): ( Label, List[ Interval ] ) = i match {
      case Nil => ( 0, i )
      case h :: t =>
        if ( chk( h, d ) )
          // In the interval, so keep that interval active and label this `d` 1
          ( 1, i )
        else if ( h.isBefore( d ) )
          // Instance past the interval, so test next interval
          // If the instant has passed the first window check if it is in the next 
          inInterval( d, t )
        else
          // Instance before any other window, so lable 0
          ( 0, i )
    }
    inInterval( d, i )
  }

  /**
   * This function takes an ordered list of `Instant`s and labels these as being either within a anomaly
   * window (1) or not (0). The windows consist of an ordered list of `Interval`. Both the `Instant` and
   * the window lists **must** be ordered chronologically.  Both `Instant` and `Interval` have nanosecond
   * precision.
   *
   * This function is parameterized by `inInterval` which allows us to select either an exclusive or
   * inclusive test on the `Interval`'s end time-stamp. **NAB** uses an **inclusive** check within the
   * anomaly windows
   *
   * @see isInInterval
   */
  def labelInstances( inInterval: ( java.time.Instant, List[ Interval ] ) => ( Label, List[ Interval ] ) )( timeStamp: List[ java.time.Instant ], wins: List[ Interval ] ): List[ Label ] = {
    @annotation.tailrec
    def label( timeStamp: List[ java.time.Instant ], labels: List[ Label ], wins: List[ Interval ] ): List[ Label ] =
      timeStamp match {
        case Nil => labels.reverse
        case h :: t =>
          // Check if h is in the a window in wins
          val ( isIn, winst ) = inInterval( h, wins )
          // Record whether or not it is 
          label( t, isIn :: labels, winst )
      }
    label( timeStamp, List(), wins )
  }

  /**
   * This function takes an ordered list of `Instant`s and labels these as being either within a anomaly
   * window (1) or not (0). The windows consist of an ordered list of `Interval`. Both the `Instant` and
   * the window lists **must** be ordered chronologically.  Both `Instant` and `Interval` have nanosecond
   * precision.  It uses an exclusive check (final time-stamp) when labeling the anomalies.
   *
   * @see labelInstances
   */
  def labelInstanceExclusive( timeStamp: List[ java.time.Instant ], wins: List[ Interval ] ) = {
    val chk = isInInterval( checkExclusiveIn ) _
    labelInstances( chk )( timeStamp, wins )
  }

  /**
   * This function takes an ordered list of `Instant`s and labels these as being either within a anomaly
   * window (1) or not (0). The windows consist of an ordered list of `Interval`. Both the `Instant` and
   * the window lists **must** be ordered chronologically.  Both `Instant` and `Interval` have nanosecond
   * precision.  It uses an inclusive check (final time-stamp) when labeling the anomalies.
   *
   * @see labelInstances
   */
  def labelInstanceInclusive( timeStamp: List[ java.time.Instant ], wins: List[ Interval ] ) = {
    val chk = isInInterval( checkInclusiveIn ) _
    labelInstances( chk )( timeStamp, wins )
  }
  
  
  def addLabels( labelInstances: ( List[ java.time.Instant ], List[ Interval ] ) => List[ Label ] )( file: String, windows: Map[ String, List[ Interval ] ], t: Table ) = {
    import pt.inescn.utils.TableSawUtils._

    val values = List.fill( t.rowCount )( 0 )

    val column = createDoubleColumn( LABEL_H, values )
    addColumn( t, column )

    /*
    val values = List.fill( t.rowCount )( 0 )
    val timeStamps = t.dateColumn( 0 )

    val column1 = createDateTimeColumn( "datetime", null )
    val tmp = column1.get( 0 )

    val column = createDoubleColumn( "label", values )
    addColumn( t, column )
  */
  }

  import scala.util.{ Try, Success, Failure }

  type NABData = (java.time.LocalDateTime, Double, String, Option[String], Float)
  type NABResult = (java.time.LocalDateTime, Double, Double, Int )

  // https://github.com/uniVocity/csv-parsers-comparison
  // http://stackoverflow.com/questions/17126365/strongly-typed-access-to-csv-in-scala
  // https://github.com/nrinaudo/kantan.csv
  // http://nrinaudo.github.io/kantan.csv/
  // X https://github.com/FasterXML/jackson-dataformat-csv
  // https://github.com/FasterXML/jackson-dataformats-text
  // https://github.com/marklister/product-collections
  def loadData( fileName: String ): Try[ Table ] = {
    import com.github.lwhite1.tablesaw.api.Table
    import com.github.lwhite1.tablesaw.io.csv.CsvReader

    Try( Table.createFromCsv( fileName, true ) )
  }

  // TODO: Table saw may cause problems. Holds date/time as an integer !?!?!?
  def addLabelsX( labelInstances: ( List[ java.time.Instant ], List[ Interval ] ) => List[ Label ] )
                          ( t: Table, wins: List[ Interval ] ): Table = {
    import pt.inescn.utils.TableSawUtils._
    wins match {
      case Nil =>
        val values = List.fill( t.rowCount )( 0 )
        val column = createDoubleColumn( LABEL_H, values )
        addColumn( t, column )
        t
      case h :: tl =>
        import com.github.lwhite1.tablesaw.columns.packeddata.PackedLocalDateTime
        val timeStamp = t.dateColumn(TIMESTAMP_H).data.asScala.toList
                                  .map { x => PackedLocalDateTime.asLocalDateTime(x.toLong) }
                                  .map { x => x.atZone( zoneID_UTC ).toInstant() }
        val values = labelInstanceInclusive( timeStamp, wins)
        val column = createDoubleColumn( LABEL_H, values )
        addColumn( t, column )
        t
    }
  }

  // TODO
  def addDetectionX( t: Table, algo: Double => Double ) = {
    //val timestamps = 
  }

  // TODO
  def saveResultsX( t: Table ) = ???

  /**
   *
   */
  def evaluateAlgo( windows: Map[ String, List[ Interval ] ], algo: Double => Double ): Iterable[ Try[ Table ] ] = {
    // TODO: par
    windows.map {
      case ( k, wins ) =>
        val data = loadData( k )
        data match {
          case f @ Failure( _ ) => f
          case Success( t ) => Try( addLabelsX( labelInstanceInclusive )( t, wins ) )
            .flatMap { t => addDetectionX( t, algo ) }
            .flatMap { t => saveResultsX( t ) }
        }
    }

  }

}
