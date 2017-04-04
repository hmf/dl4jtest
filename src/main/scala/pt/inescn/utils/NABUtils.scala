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
  def allFiles( dirName: String ): Option[ List[ File ] ] = {
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
  def allDataFiles( dataDir : String = data_dir, ext: String = "csv" ): Option[ List[ File ] ] =
    allFiles( dataDir ).map { _.filter { _.extension( includeDot = false ).exists { e => e.equals( ext ) } } }

  /**
   *  Get all the label files
   */
  def allLabelFiles( labelDir : String = label_dir, ext: String = "json" ): Option[ List[ File ] ] =
    allFiles( labelDir ).map { _.filter { _.extension( includeDot = false ).exists { e => e.equals( ext ) } } }

  /*
(root/"tmp"/"diary.txt")
  .createIfNotExists()  
  */

  import org.threeten.extra.Interval

  import java.time.Instant
  import kantan.csv._
  //import kantan.csv.ops._
  import kantan.csv.java8._
  import java.time.format.DateTimeFormatter
  import java.time.ZoneOffset

  /*
   *  Implicit date parser for Kantan CSV library: we must provide the correct format to parse 
   *  the NAB date-time stamps found in the data and result files. Note that in these files the 
   *  time-stamps have only precision up to the second. The result (JSON) labels however use  
   *  microseconds. We have opted to allow for microsecond precision -  however we do not require 
   *  the input to have full precision (see [[java.time.format.DateTimeFormatterBuilder.appendFraction]]) 
   */

  /* 
   * Make sure we can parser the NAB dates in the data files with second precision
   */
  val instantPattern = "yyyy-MM-dd HH:mm:ss" // Data files

  // Original decoder that used only second precision. 
  //val format = DateTimeFormatter.ofPattern( instantPattern ).withZone( ZoneOffset.UTC )
  //implicit val decoder: CellDecoder[ Instant ] = instantDecoder( format )

  /**
   * We use Kantan Scala library to parse CSV. It allows us to use decoders and encoders to
   * provide type-safety coding. We have opted to use the Jackson back-end which is also used for
   * the JSON parsing. This format is used to define a Kantan decoder that is the activated implicitly.
   * Note that we use  `appendFraction` t enable optional microsecond precision.
   *
   * @see https://nrinaudo.github.io/kantan.csv/
   * @see https://github.com/nrinaudo/kantan.csv
   * @see https://github.com/FasterXML/jackson
   * @see https://github.com/FasterXML/jackson-dataformat-csv
   * @see https://github.com/FasterXML/jackson-dataformats-text
   * @see see [[java.time.format.DateTimeFormatterBuilder.appendFraction]]
   * @see  http://stackoverflow.com/questions/30103167/jsr-310-parsing-seconds-fraction-with-variable-length
   * @see https://github.com/uniVocity/csv-parsers-comparison
   * @see http://stackoverflow.com/questions/17126365/strongly-typed-access-to-csv-in-scala
   * @see https://github.com/marklister/product-collections
   */
  val format = new java.time.format.DateTimeFormatterBuilder()
    .appendPattern( instantPattern )
    .appendFraction( java.time.temporal.ChronoField.MICRO_OF_SECOND, 0, 6, true )
    .toFormatter()
    .withZone( ZoneOffset.UTC )
  implicit val decoder: CellDecoder[ Instant ] = instantDecoder( format )

  /*
   *  Implicit date parser for JSON4s. The code below allows us to read and parse the NAB JSON
   *  files that contain the anomaly failures labeled by time.-stamp pairs (intervals)
   */

  import org.json4s._
  import org.json4s.jackson.JsonMethods._
  // import org.json4s.native.JsonMethods._

  /**
   * Valid formats for the time-stamp parsing.
   */
  //val dtFormatter = "yyyy-MM-dd HH:mm:ss.SSSSSS"
  //val dtFormatter = "yyyy-MM-dd HH:mm:ss.SSS"

  //implicit val formats = DefaultFormats // Brings in default date formats etc.

  /**
   *  Create your own formatter that overrised's JSON4s formatters. Do this according to the JSON4S
   *  documentation. Note that this only works for [[java.util.Date]] that only has millisecond precision. In order
   *  to have microsecond precision we have opted to use the JDK 8 API. See [[StringToJDKLocalDateTime]] for an example of how to circumvent this.
   *
   * @see http://stackoverflow.com/questions/27408180/json4s-conversion-to-java-sql-timestamp-does-not-work
   * @see https://gist.github.com/djamelz/f963cab45933d05b2e10d8680e4213b6
   *
   * implicit val formats = new DefaultFormats {
   * import java.text.SimpleDateFormat
   * // https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
   * override def dateFormatter = {
   * val f = new SimpleDateFormat( dtFormatter )
   * f.setTimeZone( DefaultFormats.UTC )
   * f
   * }
   * }
   */

  /**
   * Labeling JSON files: NAB uses time-stamps with microsecond precision. Note
   * also that this is a LocateDatTime because it does not contain the ISO Zone ID.
   */
  val datePattern = "yyyy-MM-dd HH:mm:ss.SSSSSS"
  val NABformatter = java.time.format.DateTimeFormatter.ofPattern( datePattern )

  /**
   * JSon4S that is used to parse the time-stamp an assigns it the type `java.time.LocalDateTime`.
   * Note that the patter used expects all digits of the microseconds to be present.
   *
   * @see java.time.LocalDateTime
   */
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

  /**
   * JSon4S that is used to parse the time-stamp an assigns it the type `java.time.Instant`.
   * Note that the patter used expects all digits of the microseconds to be present.
   * Note also that to do this we must provide a `ZoneId` for an `Instant`We have chosen
   * to use the UTC zone ID.
   *
   * @see java.time.Instant
   * @see java.time.ZoneId
   */
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

  /**
   * This function takes in a `better.files.File` and uses a JSON parser to parse and load the NAB
   * label data (anomaly window). We assume all dates are instances (a local date with the UTC time zone).
   * The NAB anomaly windows consists a list of lists. Each inner list contains two local dates (no zone
   * information).  Each such pair defines an anomaly window which is modeled here as an `Interval`. The list
   * of all intervals is the set of all labeled anomalies. We index each file by its name, so we return a map
   * (keyed by filename) that points to a list of label windows.
   *
   * Several libraries for JSON parsing exist. We opted for Json4S, which is a front end for various
   * Java JSON parsers. It provides us with an easy means of adapting the parsing of elements. More
   * specifically we use implicits to define and parse the time-stamps correctly.
   *
   * Care was taken to ensure proper parsing to the millisecond. The JDK 8 and threeten-extra were used
   * to ensure that all time-stamps have nanosecond precision.
   *
   * @see [[StringToJDKInstant]]
   * @see http://json4s.org/
   * @see http://www.threeten.org/threeten-extra/apidocs/org/threeten/extra/Interval.html
   * @see https://www.playframework.com/documentation/2.4.x/ScalaJson
   * @see https://github.com/lift/framework/tree/master/core/json
   * @see http://spray.io/
   * @see https://github.com/fommil/spray-json-shapeless
   * @see http://argonaut.io/
   * @see https://github.com/circe/circe
   * @see https://github.com/sphereio/sphere-scala-libs/tree/master/json (uses json4s)
   * @see https://github.com/non/jawn
   * @see https://github.com/propensive/rapture/blob/dev/doc/json.md
   * @see https://github.com/scala/scala-parser-combinators
   * @see https://github.com/julienrf/play-json-derived-codecs
   * @see https://github.com/mandubian/play-json-zipper
   * @see https://github.com/rjmac/rojoma-json
   * @see https://www.mkyong.com/java8/java-8-how-to-convert-string-to-localdate/
   */
  def loadJSONLabels( file: File ) = {
    // JSON parsing
    import org.json4s._
    import org.json4s.native.JsonMethods._

    implicit val formats = DefaultFormats + StringToJDKInstant

    //val file = io.Source.fromFile( fileName )
    val json = parse( file.contentAsString )

    val raw = json.extract[ Map[ String, List[ List[ java.time.Instant ] ] ] ]
    windowToIntervals( raw )
  }

  /**
   * Converts two dates into an Interval. We use an interval that has nanosecond precision. We
   * looked at several options but in the end tried to stick to the JDK 8 API. However this API
   * does not have the concept of `Interval` hence the use of the `threeten-extra`package. See
   * below several links of interest.
   *
   * @see org.threeten.extra.Interval
   * @see https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
   * @see http://www.threeten.org/threeten-extra/
   * @see http://www.threeten.org/threeten-extra/apidocs/org/threeten/extra/Interval.html
   * @see http://www.joda.org/joda-time/
   * @see https://github.com/MenoData/Time4J
   * @see https://github.com/nscala-time/nscala-time
   * @see https://github.com/reactivecodes/scala-time
   * @see http://www.lamma.io/
   * @see https://github.com/maxcellent/lamma
   * @see https://www.hackingnote.com/en/scala/datetime/
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

  /**
   * Checks if `Instant` `d` is within the `Interval` `i`.
   * Test is inclusive with the start time-stamp and exclusive with the end time-stamp.
   */
  def checkExclusiveIn( i: Interval, d: java.time.Instant ) = i.contains( d )

  /**
   * Checks if `Instant` `d` is within the `Interval` `i`.
   * Test is inclusive both with the start and the end time-stamps.
   */
  def checkInclusiveIn( i: Interval, d: java.time.Instant ) = i.contains( d ) || ( i.getEnd.compareTo( d ) == 0 )

  /**
   * The labels are integers either  0 (no failure) or 1 (failure)
   */
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
   * @see [[pt.inescn.utils.NABUtilsSpec]]
   * @see https://github.com/numenta/NAB
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

  /*
   * The following code is used to represent the NAB data (date, labels and results). These classes are used to 
   * encode and decode  data to and from the CSV files.  We also include a set of helper functions that facilitate
   * copying and and converting the data. 
   */

  import scala.util.{ Try, Success, Failure }
  import shapeless.{ :: => *::, HList, HNil }
  import kantan.csv.ReadError

  /**
   * Basic NAB data consists of a single sensor data. It has two columns: a time-stamp
   * and a value represented as a double.  This class represents a single row of the CSV file.
   */
  case class NABDataRow( dt: java.time.Instant, value: Double )
  /**
   *  This class represents a single row of the NAB CSV result file. The NAB results include the following
   *  additional fields:
   * @param anomaly_score: Double - the NAB runs a threshold optimizer and uses that to convert the
   * `raw_score` to this `anomaly_score`. This is used evaluate the detector's performance.
   * @param raw_score: Double - are the scores generated by the detector and has a value between <0.0 ... 1.0>.
   * We can use this directly without the NAB threshold optimizer directly. However we must assign a threshold
   * to our detector found in the NAB  `config/thresholds.json` file. This defaults to 0.5.
   * @param label: Int - This has a value of 1 if the time-stamp is within the label's anomaly window. In other
   * words, this value does **not** depend on the detector's output.
   * @param reward_low_FP_rate: Double - score when we assign greater weight to the low FPs
   * @param reward_low_FN_rate: Double - score when we assign greater weight to the low FNs
   * @param standard: Double - score when we assign the same weight to the low FNs and FPs
   */
  case class NABResultRow( dt: java.time.Instant, value: Double, anomaly_score: Double, raw_score: Double, label: Int, reward_low_FP_rate: Double, reward_low_FN_rate: Double, standard: Double )

  /**
   * The result of converting the NAB data rows in [[NABData]] to columns.
   * @see NABData
   */
  case class NABFrame( dt: List[ java.time.Instant ], value: List[ Double ] )
  /**
   * The result of adding the labels column to the [[NABFrame]] columns. Represents the
   * NAB data after it has been labeled.
   * @see NABData
   * @see NABFrame
   */
  case class NABFrameLabelled( dt: List[ java.time.Instant ], value: List[ Double ], label: List[ Int ] )
  /**
   * The result of converting the NAB result rows in [[NABFinalResult]] to columns. Note that not all
   * the fields exists. This represents an intermediary result after the detector has been applied and
   * the data labeled.
   * @see NABFinalResult
   */
  case class NABResult( dt: List[ java.time.Instant ], value: List[ Double ], anomaly_score: List[ Double ], label: List[ Int ] )
  /**
   * Represents the results data as is generated by NAB. It contains the row data in [[NABFinalResult]]
   * converted to columns.
   */
  case class NABResultAll( dt: List[ java.time.Instant ], value: List[ Double ], anomaly_score: List[ Double ], raw_score: List[ Double ], label: List[ Int ],
                           reward_low_FP_rate: List[ Double ], reward_low_FN_rate: List[ Double ], standard: List[ Double ] )

  /**
   * Utilities to facilitate manipulating the NAb files. Provides easier type checking.
   */
  object NABDataRow {

    /**
     * Create an empty column major NAB data container
     */
    val emptyNABFrame = NABFrame( List[ java.time.Instant ](), List[ Double ]() )

    /**
     * Create an empty column major NAB results container (NAB final output)
     */
    val emptyNABResultAll = NABResultAll( List[ java.time.Instant ](), List[ Double ](), List[ Double ](),
      List[ Double ](), List[ Int ](), List[ Double ](),
      List[ Double ](), List[ Double ]() )

    /**
     * Add a `NABDataRow`to the `NABFrame` in column major form.
     */
    def addTo( nabf: NABFrame, e: NABDataRow ) = NABFrame( e.dt :: nabf.dt, e.value :: nabf.value )

    /**
     * Add a `NABResultRow`to the `NABResultAll` in column major form.
     */
    def addTo( acc: NABResultAll, e: NABResultRow ) =
      NABResultAll( e.dt :: acc.dt, e.value :: acc.value, e.anomaly_score :: acc.anomaly_score,
        e.raw_score :: acc.raw_score, e.label :: acc.label,
        e.reward_low_FP_rate :: acc.reward_low_FP_rate,
        e.reward_low_FN_rate :: acc.reward_low_FN_rate, e.standard :: acc.standard )

    /**
     * When converting the rows to columns we stack the `NABDataRow` data in reverse order.
     * This reverses those columns to get the correct order back.
     */
    def reverse( nabf: NABFrame ) = NABFrame( nabf.dt.reverse, nabf.value.reverse )

    /**
     * When converting the rows to columns we stack the `NABResultAll` data in reverse order.
     * This reverses those columns to get the correct order back.
     */
    def reverse( nabf: NABResultAll ) =
      NABResultAll( nabf.dt.reverse, nabf.value.reverse, nabf.anomaly_score.reverse,
        nabf.raw_score.reverse, nabf.label.reverse,
        nabf.reward_low_FP_rate.reverse,
        nabf.reward_low_FN_rate.reverse, nabf.standard.reverse )
  }

  import NABDataRow._

  /* TODO: how to solve this?
    //def conv[C, B](x : Throwable) : Either[C, B] = Left( List(x) ) 
    def conv[B](x : Throwable) : Either[List[Throwable], B] = Left( List(x) ) 

    def doX1[A,B,C](a: A, g: A => Try[A], f : A => Either[C,B], conv:Throwable  => Either[C,B] ) : Either[C,B] = {
      val t0 = g(a)
      val tt  = t0.fold( { x => conv(x) } , { x => f(x) } )
      tt
    }
    
    def doX2[A,B,C](a: A, g: A => Try[A], f : A => Either[C,B]) : Either[C,B] = {
      def conv(x : Throwable) : Either[C, B] = Left( List(x) ) 
      val t0 = g(a)
      val tt  = t0.fold( { x => conv(x) } , { x => f(x) } )
      tt
    }
    
    def doX3[A,B,C](a: A, g: A => Try[A], f : A => Either[C,B]) : Either[C,B] = {
      val t0 = g(a)
      val tt  = t0.fold( { x => conv(x) } , { x => f(x) } )
      tt
    }
    */

  /**
   * This is a helper function that converts the `kantan.csv.ReadResult`(Success or Failure) to
   * a `Either[ List[ Throwable ], B ]`. This allows us to collect the Kantan read failures that
   * store an Exception and report those at the end. Specialized function will also be provided to
   * deal with the case of `Success`.
   */
  def conv[ B ]( x: Throwable ): Either[ List[ Throwable ], B ] = Left( List( x ) )

  /**
   * This s a generic (polymorphic) function that allows us read a [[better.files.File]], parse its contents
   * using the Kasntan CSV parser. The parser uses the `RowDecoder[ A ]` that is implicitly passed onto it.
   * The parser returns a list of roes that were either successfully (contain data) or unsuccessfully read
   * (contains a Throwable).  If the data row was unsuccessfully read then we collect these as a list of
   * `Left[ List[ Throwable ]]`. If the data is successfully read then it is collected as a `Right[ B ]`
   * where be can be for example a `NABFrame` or a `NABResultAll`. These conversions to columns
   * are done by the `toColumns` function parameter.
   *
   * Note that is the CSV file cannot be read and parsed then the Exception is converted (via
   * `conv` utility function) to a  `Left[ List[ Throwable ]]` type.
   *
   * @see toNABFrameColumns
   * @see toNABResultAllColumns
   */
  def load[ A, B, C ]( f: File )( toColumns: kantan.csv.CsvReader[ kantan.csv.ReadResult[ A ] ] => Either[ List[ Throwable ], B ] )( finishColumns: B => B )( implicit dt: RowDecoder[ A ] ): Either[ List[ Throwable ], B ] = {
    import kantan.csv._
    import kantan.csv.ops._

    val reader = Try {
      val rawData = f.toJava
      rawData.asCsvReader[ A ]( rfc.withHeader )
    }
    reader.fold( { x => conv( x ) }, { x => toColumns( x ) } )
      .map { x => finishColumns( x ) }
  }

  /**
   * This is a specific implementation of `load`s `toColumns` function parameter. It reads in `NABDataRow`s
   * and collects these into columns major data as a `NABFrame`. If any error occurred during the parsing of
   * a row, then only the failures are returned.
   *
   * @see load
   */
  def toNABFrameColumns( reader: kantan.csv.CsvReader[ kantan.csv.ReadResult[ NABDataRow ] ] ): Either[ List[ Throwable ], NABFrame ] = {
    val z = ( emptyNABFrame, List[ Throwable ]() )
    val tmp = reader.foldLeft( z ) {
      case ( ( acc, el ), kantan.codecs.Result.Success( e ) ) => ( addTo( acc, e ), el )
      case ( ( acc, el ), kantan.codecs.Result.Failure( e ) ) => ( acc, e :: el )
    }
    if ( !tmp._2.isEmpty ) Left( tmp._2.reverse ) else Right( reverse( tmp._1 ) )
  }

  /**
   * Loads the NAB data as rows (`NABDataRow`) and converts it to column major format (`NABFrame`)
   *
   * @see [[load]]
   * @see [[NABDataRow]]
   * @see [[NABFrame]]
   */
  def loadData( file: File )( implicit dt: RowDecoder[ NABDataRow ] ): Either[ List[ Throwable ], NABFrame ] = {
    load( file )( toNABFrameColumns )( x => x )
  }

  /**
   * This is a specific implementation of `load`s `toColumns` function parameter. It reads in `NABResultRow`s
   * and collects these into columns major data as a `NABResultAll`. If any error occurred during the parsing of
   * a row, then only the failures are returned.
   *
   * @see load
   */
  def toNABResultAllColumns( reader: kantan.csv.CsvReader[ kantan.csv.ReadResult[ NABResultRow ] ] ): Either[ List[ Throwable ], NABResultAll ] = {
    val z = ( emptyNABResultAll, List[ Throwable ]() )
    val tmp = reader.foldLeft( z ) {
      case ( ( acc, es ), kantan.codecs.Result.Success( e ) ) => ( addTo( acc, e ), es )
      case ( ( acc, es ), kantan.codecs.Result.Failure( e ) ) => ( acc, e :: es )
    }
    val r = tmp._1
    if ( !tmp._2.isEmpty ) Left( tmp._2.reverse ) else Right( NABResultAll( r.dt.reverse, r.value.reverse, r.anomaly_score.reverse, r.raw_score.reverse, r.label.reverse,
      r.reward_low_FP_rate.reverse, r.reward_low_FN_rate.reverse, r.standard.reverse ) )
  }

  /**
   * Loads the NAB results as rows (`NABResultRow`) and converts it to column major format (`NABResultAll`)
   *
   * @see [[load]]
   * @see [[NABDataRow]]
   * @see [[NABFrame]]
   */
  def loadResults( file: File )( implicit dt: RowDecoder[ NABResultRow ] ): Either[ List[ Throwable ], NABResultAll ] = {
    load( file )( toNABResultAllColumns )( x => x )( dt )
  }

  /**
   * This function takes in a column major `NABFrame` that is not labeled and a list anomaly windows
   * (`Interval`).  It then uses the `labelInstances` function parameter to label each  `Instant` time-stamp
   * in the  NABFrame as 1 if it is in one of the anomaly windows or 0 if it s not.
   *
   * @see [[NABFrame]]
   * @see [[NABFrameLabelled]]
   */
  def addLabels( labelInstances: ( List[ java.time.Instant ], List[ Interval ] ) => List[ Label ] )( t: NABFrame, wins: List[ Interval ] ): NABFrameLabelled = {
    wins match {
      case Nil =>
        val values = List.fill( t.dt.length )( 0 )
        NABFrameLabelled( t.dt, t.value, values )
      case h :: tl =>
        val values = labelInstanceInclusive( t.dt, wins: List[ Interval ] )
        NABFrameLabelled( t.dt, t.value, values )
    }
  }

  /**
   * Converts a [[java.time.Instant]] to bytes. We do this by converting the time-stamp to
   * its string representation and then convert that string to an [[Array[Byte]]].
   */
  def toBytes( i: Instant ) = i.toString.getBytes( "UTF-8" )

  import java.nio.ByteBuffer
  import java.lang.{ Double => JDouble }
  //import java.lang.{ Byte => JByte }

  val doubleSize = JDouble.BYTES 

  /**
   * Converts a Double to an [[Array[Byte]]]. Care is take to set the buffer size using
   * JDK 8's primitive `SIZE` value. We also `flip()` the array so that we can reset the
   * pointers for reading (output).
   *
   * @see http://stackoverflow.com/questions/9810010/scala-library-to-convert-numbers-int-long-double-to-from-arraybyte
   */
  def toBytes( v: Double ) = {
    val bf = java.nio.ByteBuffer.allocate( doubleSize )
    bf.putDouble( v )
    bf.flip()
    //bf.order(java.nio.ByteOrder.nativeOrder)
    bf.array()
  }

  import java.security.MessageDigest

  /**
   * This function converts the time-stamp an array bytes and then updates the digest
   * (passed on implicitly) with his data.
   *
   * @see  [[toBytes(i: Instant)]]
   */
  def updateHash( dt: Instant )( implicit digest: MessageDigest ) = digest.update( toBytes( dt ) )

  /**
   * This function converts the time-stamp an array bytes and then updates the digest
   * (passed on implicitly) with his data.
   *
   * @see  [[toBytes(value : Double)]]
   */
  def updateHash( value: Double )( implicit digest: MessageDigest ) = digest.update( toBytes( value ) )

  /**
   * This function reads the NAB result file (contains [[NABResultRow]]) and uses the first
   * `sample_size` (fraction of the total number of lines, default set to 0.15). It then generates the
   * hash  of first all of these initial time-stamps and then all of the initial values. It calculates the
   * has using the `digest` that is passed in as an implicit.
   *
   * Note: we need to bring in the Kantan shapeless decoders/encoders using in order to have the
   * case reading implicit available:
   * {{{
   * // https://nrinaudo.github.io/kantan.csv/tut/shapeless.html
   * import kantan.csv._
   * import kantan.csv.ops._
   * import kantan.csv.generic._
   * }}}
   * 
   * @see updateHash(dt : Instant)
   * @see updateHash(value : Double)
   * @see https://softwarecave.org/2014/02/26/calculating-cryptographic-hash-functions-in-java/
   * @see https://docs.oracle.com/javase/8/docs/api/java/security/MessageDigest.html
   * @see https://www.mkyong.com/java/java-sha-hashing-example/
   * @see https://github.com/alphazero/Blake2b
   */
  def tagData( f: File, sample_size: Double = 0.15 )( implicit dt: RowDecoder[ NABResultRow ], digest: MessageDigest ): Either[ List[ Throwable ], Array[ Byte ] ] = {
    val results = loadResults( f )
    //implicit val digest = MessageDigest.getInstance("SHA-256")
    //println(digest.getAlgorithm)
    results.flatMap { x =>
      val size = x.dt.length
      val sample_len = ( size * sample_size ).toInt
      x.dt.take( sample_len ).map { x => updateHash( x ) }
      x.value.take( sample_len ).map { x => updateHash( x ) }
      Right( digest.digest )
    }
  }

  object Hex {
    def valueOf( buf: Array[ Byte ] ): String = buf.map( "%02X" format _ ).mkString
  }
  
  /**
   * This function takes in a list of `better.files.File`s reads the start of these files 
   * (only the `sample_size` fraction of lines are used) and uses that data to generate a hash
   * value. This hash value will act as a key that maps to the fileName. This will allow a 
   * *perfect* detector to read the data, find the NAB result file and use that to generate the 
   * perfect score output based on the anomaly window labels. 
   * 
   * @see http://stackoverflow.com/questions/6489584/best-way-to-turn-a-lists-of-eithers-into-an-either-of-lists
   */
  def tagFiles(files : List[File], sample_size: Double = 0.15 )( implicit dt: RowDecoder[ NABResultRow ], digest: MessageDigest ) 
  :  Either[List[Throwable], Map[Array[Byte], String]] = {
    val t = files.map { file => 
      val keys = tagData(file, sample_size) 
      //println(keys)
      keys.map(hash => (hash, file.nameWithoutExtension) )
      }
    val t3 = t.partition(_.isLeft)
    t3 match {
      case (Nil, right) => Right( right.flatMap { x => List( x.right.get ) } .toMap )
      case (left, _) => Left( left.flatMap( { x => x.left.get } ) )
    }
  }

  // TODO
  def addDetection( t: NABFrameLabelled, algo: Double => Double ): Try[ NABResult ] = Try {
    val anomaly_score = t.value.map { x => algo( x ) }
    NABResult( t.dt, t.value, anomaly_score, t.label )
  }

  // TODO
  def saveResults( fileName: String, t: NABResult ): Try[ String ] = ???

  /**
   *
   */
  // TODO: incorrect return type
  def evaluateAlgo( labelledData: Map[ String, List[ Interval ] ], algo: Double => Double )( implicit dt: RowDecoder[ NABDataRow ] ) = {
    import better.files._
    import better.files.Cmds._
    // TODO: par
    labelledData.map {
      case ( k, wins ) =>
        val rawData = cwd / "data/nab/data/artificialWithAnomaly" / k
        val data = loadData( rawData )
        data match {
          case Left( e ) => Left( e.toString )
          case Right( t ) =>
            Try( addLabels( labelInstanceInclusive )( t, wins ) )
              .flatMap { t => addDetection( t, algo ) }
              .flatMap { t => saveResults( k, t ) }
        }
    }
  }

}
