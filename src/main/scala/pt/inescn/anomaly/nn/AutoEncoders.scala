package pt.inescn.anomaly.nn

/**
 * Types
 * -------
 *  https://handong1587.github.io/deep_learning/2015/10/09/rnn-and-lstm.html
 *  
 * 
 *  
 * Zip files in Scala
 * @see http://stackoverflow.com/questions/30640627/how-to-unzip-a-zip-file-using-scala
 * @see http://stackoverflow.com/questions/5153544/how-to-read-from-zipped-xml-files-in-scala-code
 * @see https://gist.github.com/Swind/2568527
 * @see https://gist.github.com/Swind/2601206
 * 
 * Tar file sin Java/Scala
 * @see https://gist.github.com/ayosec/6853615
 * @see http://stackoverflow.com/questions/315618/how-do-i-extract-a-tar-file-in-java
 * @see http://commons.apache.org/proper/commons-compress/
 * @see http://commons.apache.org/proper/commons-vfs/filesystems.html
 * @see https://truezip.java.net/
 * @see http://stackoverflow.com/questions/14940099/how-to-read-gzipd-file-in-scala
 * @see http://thinktibits.blogspot.pt/2013/01/read-extract-tar-file-java-example.html
 * 
 * 
 * IMS Bearing Data
 * ______________
 * Support from Rexnord Corp. in Milwaukee, WI
 * 1. Each individual files that are 1-second vibration signal snapshots recorded at specific intervals
 * 2. Each file consists of 20,480 points with the sampling rate set at 20 kHz
 * 3. File name indicates when the data was collected (every 20 minutes)
 * 4. Each record (row) in the data file is a data point. 
 * 5. Data collection was facilitated by NI DAQ Card 6062E. 
 * 6. Larger intervals of time stamps (showed in file names) indicate resumption of the experiment in the next 
 *    working day
 *
 * "A magnetic plug installed in the oil feedback pipe collects debris from the oil as evidence of bearing degradation. 
 * The test will stop when the accumulated debris adhered to the magnetic plug exceeds a certain level and causes 
 * an electrical switch to close."
 *  
 *  Experiment is carried out for 35 days. After day 30 onset of failure begins. Once failure starts it is rapid 
 *  (order of 3 days). The failures can be observed as "weak" periodic impulses. 
 *  
 *  Best for RUL
 *  
 * Hai Qiu, Jay Lee, Jing Lin. “Wavelet Filter-based Weak Signature Detection Method and its Application on Roller 
 * Bearing Prognostics.” Journal of Sound and Vibration 289 (2006) 1066-1090
 * 
 * In Science Direct:
 * 311 citing articles
 * At least 3 recommendation 
 * - Robust performance degradation assessment methods for enhanced rolling element bearing prognostics
 *    Hai Qiua , Jay Leeb, , Jing Linc, Gang Yud, 
 *    http://dx.doi.org/10.1016/j.aei.2004.08.001
 *    Same, but also uses SOM
 *    
 *  - Rolling element bearing faults diagnosis based on optimal Morlet wavelet filter and autocorrelation 
 *    enhancement
 *    Wensheng Su, Fengtao Wang, , Hong Zhu, Zhixin Zhang, Zhenggang Guo
 *    http://dx.doi.org/10.1016/j.ymssp.2009.11.011  
 *    data - Case Western Reserve University Bearing Data Center Website (see http://csegroups.case.edu/bearingdatacenter)
 *    
 *    A Comparative Study of Various Methods of Bearing Faults Diagnosis Using the Case Western Reserve University Data
 *    Adel Boudiaf, Abdelkrim Moussaoui, Amine Dahane, Issam Atoui
 *    http://link.springer.com/article/10.1007/s11668-016-0080-7
 *    DOI: 10.1007/s11668-016-0080-7
 *    
 *    Bearing fault detection based on optimal wavelet filter and sparse code shrinkage
 *    Wei He, Zhi-Nong Jiang, , Kun Feng
 *    http://dx.doi.org/10.1016/j.measurement.2009.04.001
 *    
 * @see https://ti.arc.nasa.gov/tech/dash/pcoe/prognostic-data-repository/
 * @see http://data-acoustics.com/measurements/bearing-faults/bearing-4/
 * @see http://www.imscenter.net/
 * @see http://www.sciencedirect.com/science/article/pii/S0022460X0500221X
 * @see http://csegroups.case.edu/bearingdatacenter/pages/welcome-case-western-reserve-university-bearing-data-center-website
 * 
 * FemtoBearing 
 * __________
 * 
 * 1. Radial force applied on the bearing, 
 *    Rotation speed of the shaft handling the bearing
 *    Torque inflicted to the bearing. 
 *    All three acquired at a frequency equal to 100 Hz.
 * 2. 2 accelerometers sampled at 25.6 kHz 
 * 3. 1 temperature sensor sampled at 10 Hz.
 * 4. 6 fields in total
 * 
 * "3 different loads were considered:
 * • First operating conditions: 1800 rpm and 4000 N;
 * • Second operating conditions: 1650 rpm and 4200 N;
 * • Third operating conditions: 1500 rpm and 5000 N.
 * Participants are provided with 6 run-to-failure datasets, and are asked to estimate accurately the RUL of 
 * 11 remaining bearings"
 * 
 * File naming convention:
 * 1. BearuingX_Y
 * 2. X - Operating condition
 * 3. Y - Bearing ID
 * 
 * Datasets: training set, validation set and a full (test?) data set (see original link). 
 * 
 * "According to the bearing and to the way the degradation evolves, the fault modes can be slightly different for 
 * distinct bearings." Note: failures may come from bearings, inner or outer races. 
 * 
 * Best for RUL ("... IEEE PHM 2012 Prognostic Challenge. The challenge is focused on prognostics of the remaining 
 * useful life (RUL) ...")
 * 
 * PRONOSTIA : An experimental platform for bearings accelerated degradation tests. Patrick Nectoux, Rafael 
 * Gouriveau, Kamal Medjaher, Emmanuel Ramasso, Brigitte Chebel-Morello, Noureddine Zerhouni, 
 * Christophe Varnier
 * 
 * @see http://www.femto-st.fr/en/Research-departments/AS2M/Research-groups/PHM/IEEE-PHM-2012-Data-challenge.php
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
 * General References
 * Sequential Feature Explanations for Anomaly Detection
 * Md Amran Siddiqui and Alan Fern and Thomas G. Dietterich and Weng-Keen Wong
 * @see http://web.engr.oregonstate.edu/~afern/papers/odd15-siddiqui.pdf
 * 
 * Anomaly Detection Using Replicator Neural Networks Trained on Examples of One Class
 * Hoang Anh Dau, Vic Ciesielski, and Andy Song
 * 
 * Datasets
 * [anomaly detection benchmark data]
 * Evaluating Real-time Anomaly Detection Algorithms - the Numenta Anomaly Benchmark
 * Alexander Lavin, Subutai Ahmad
 * @see https://numenta.com/numenta-anomaly-benchmark/
 * @see https://github.com/numenta/NAB
 * @see https://arxiv.org/abs/1510.03336
 * 
 * https://www.researchgate.net/post/What_are_anomaly_detection_benchmark_datasets
 * @see http://kdd.ics.uci.edu/databases/kddcup99/kddcup99.html
 * 
 *  University of New Mexico (UNM) dataset
 * @see http://www.cs.unm.edu/~immsec/systemcalls.htm
 * 
 * Concept drift
 * @see http://www.liaad.up.pt/kdus/products/datasets-for-concept-drift
 * 
 * Outliers
 * @see http://homepage.tudelft.nl/n9d04/occ/index.html
 * @see http://www.dbs.ifi.lmu.de/research/outlier-evaluation/
 * 
 * Mixed
 * @see https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OPQMVF
 * @see https://yahooresearch.tumblr.com/post/114590420346/a-benchmark-dataset-for-time-series-anomaly
 * @see https://webscope.sandbox.yahoo.com/catalog.php?datatype=s&did=70
 * @see http://odds.cs.stonybrook.edu/
 * 
 * Social Networks
 * @see https://obj.umiacs.umd.edu/tagged_social_spam/index.html
 * 
 * A Comparative Evaluation of Unsupervised Anomaly Detection Algorithms for Multivariate Data
 * Markus Goldstein,Seiichi Uchida
 * http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0152173
 * @see https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OPQMVF
 * 
 * Video
 * UCSD Anomaly Detection Dataset 
 * @see http://www.svcl.ucsd.edu/projects/anomaly/dataset.htm
 * University of Minnesota unusual crowd activity dataset
 * @see http://mha.cs.umn.edu/Movies/Crowd-Activity-All.avi
 * Signal Analysis for Machine Intelligence 
 * @see http://vision.eecs.yorku.ca/research/anomalous-behaviour-data/
 * For anomaly detection in surveillance videos
 * Virat video dataset 
 * @see http://www.viratdata.org/
 * McGill University
 * @see http://www.cim.mcgill.ca/~javan/index_files/Dominant_behavior.html 
 * 
 * [anomaly detection time series dataset]
 * @see https://yahooresearch.tumblr.com/post/114590420346/a-benchmark-dataset-for-time-series-anomaly
 * @see https://github.com/twitter/AnomalyDetection
 * @see https://cran.r-project.org/web/packages/tsoutliers/index.html
 * @see http://conservancy.umn.edu/bitstream/handle/11299/92985/?sequence=1
 * 
 * Physiogical Signals
 * [ECG]
 * @see https://www.physionet.org/physiobank/database/
 * @see https://archive.ics.uci.edu/ml/datasets/Arrhythmia
 * @see http://www.cs.ucr.edu/~eamonn/discords/
 * @see http://cvrgrid.org/data/available-datasets
 * @see http://www.cs.cmu.edu/~bobski/data/data.html
 * @see http://peterhcharlton.github.io/RRest/datasets.html
 * @see https://pbil.univ-lyon1.fr/ade4/ade4-html/ecg.html
 * @see http://www.ecg-imaging.org/data-archive
 * @see http://edgar.sci.utah.edu/
 * @see http://mlpr.wikidot.com/pattern-recognition-datasets
 * @see http://orestibanos.com/datasets.htm
 * @see http://timeseriesclassification.com/dataset.php
 * 
 * [Sensor Data]
 * IoT 
 * @see http://www.datasciencecentral.com/profiles/blogs/great-sensor-datasets-to-prepare-your-next-career-move-in-iot-int
 * @see https://www.ncdc.noaa.gov/
 * @see https://archive.ics.uci.edu/ml/datasets.html?format=&task=&att=&area=&numAtt=&numIns=&type=ts&sort=nameUp&view=list
 * @see http://iot.ee.surrey.ac.uk:8080/datasets.html
 * @see http://www.cis.fordham.edu/wisdm/dataset.php
 * @see http://www.cis.fordham.edu/wisdm/dataset.php#actitracker
 * @see http://ailab.wsu.edu/casas/datasets/
 * @see http://db.csail.mit.edu/labdata/labdata.html
 * @see https://sites.google.com/a/drwren.com/wmd/
 * 
 * http://www.bioshare.info/en/audacity ?????
 * https://www.ncbi.nlm.nih.gov/pubmed/24377903 ???
 * 
 * https://github.com/pathikrit/better-files
 * http://www.lihaoyi.com/Ammonite/#Ammonite-Ops
 * https://github.com/lauris/awesome-scala
 * https://github.com/jesseeichar/scala-io
 * http://rapture.io/
 * https://github.com/scalaz/scalaz-stream
 * 
 * sbt "run-main pt.inescn.anomaly.nn.AutoEncoders"
 */
object AutoEncoders {

  val algorithm = "autoencoder"
  
  val data_dir = "/home/hmf/my_py2/download/NAB/data/"
  val label_dir = "/home/hmf/my_py2/download/NAB/labels"
  val results_dir = "/home/hmf/my_py2/download/NAB/results"

  val combined_labels = "combined_labels.json"
  val combined_windows = "combined_windows.json"
  val combined_windows_tiny =  "combined_windows_tiny.json"
  
  //import scala.io.Source 
  import better.files._
  import java.io.{File => JFile}
  
  /**
   * Get all files listed in the directory 
   */
  def allFiles(dirName : String) : Option[Seq[File]] = {
    val dir = File(dirName)
    if (!dir.isDirectory) 
      None
    else {
      //val matches: Iterator[File] = dir.glob("**") 
      val matches: Iterator[File] = dir.glob("**") 
      Some(matches.toList)
    }
  }

  /** 
   *  Get all the data files
   */
  def allDataFiles(ext: String = "csv") : Option[Seq[File]] = 
    allFiles(data_dir).map { _.filter { _.extension(includeDot=false).exists { e => e.equals( ext ) }  } }

  /** 
   *  Get all the label files
   */
  def allLabelFiles(ext: String = "json") : Option[Seq[File]] = 
    allFiles(label_dir).map { _.filter { _.extension(includeDot=false).exists { e => e.equals( ext ) }  } }
  
/*
(root/"tmp"/"diary.txt")
  .createIfNotExists()  
  */  
  
  import java.time.LocalDate
  //import org.threeten.extra.Interval
  import org.joda.time.Interval
  
  def runAlgo(data: File) = ???
  def addDetection(data: File, detections: List[Double]) = ???
  def addLabels(detections: List[Interval], windows: List[Interval]) = ???
  
  
  def runExp(data: File, windows:Map[String, List[Interval]]) = {
    val a = runAlgo(data)
    val d = addDetection(data, a)
    val r = addLabels(d, windows(data.nameWithoutExtension))
  }
  
  import org.json4s._
  //import org.json4s.jackson.JsonMethods._
  import org.json4s.native.JsonMethods._

  implicit val formats = DefaultFormats // Brings in default date formats etc.

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
  def readWindows(file: File) : Map[String, List[Interval]]= ???
  
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
  
  def main( args: Array[ String ] ) {
   
    //println( allDataFiles().mkString(",") )
    //println( allLabelFiles().mkString(",") )
    
    case class OneDate(t1: java.util.Date)
    case class Window(t1: java.util.Date, t2: java.util.Date)
    case class Windows(files: Map[String, List[Window]])
    
    implicit val formats = new DefaultFormats {
         import java.text.SimpleDateFormat
         // https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
         override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSSSS")
       }
    
    val json0 = parse("""{
            "t1" : "2014-04-10 16:15:00.000000"
            }
      """)
    val w0 = json0.extract[OneDate]
    println(json0)
    println(w0)
    
    val json1 = parse("""
        [
            "2014-04-10 16:15:00.000000",
            "2014-04-12 01:45:00.000000"
        ]
      """)
    println(json1)
    val w1 = json1.extract[List[java.util.Date]]
    println(w1.mkString(","))

    val json2 = parse("""[
        [
            "2014-02-19 10:50:00.000000",
            "2014-02-20 03:30:00.000000"
        ],
        [
            "2014-02-23 11:45:00.000000",
            "2014-02-24 04:25:00.000000"
        ]
      ]
      """)
    println(json2)
    val w2 = json2.extract[List[List[java.util.Date]]]
    println(w2.mkString(";"))
    
    val json3 = parse("""
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
            """)
    println(json3)
    val w3 = json3.extract[Map[ String, List[List[java.util.Date]]] ]
    println(w3.mkString(";\n"))
    
    
    val list1 = List(Some(1), None, Some(2))
    val list2 = list1.flatten // will be: List(1,2)
    val list3 = list1.flatMap { x => x }// will be: List(1,2)
    
    def windowToIntervals(windows : Map[String, List[List[java.util.Date]]]) = {
      
      def makeInterval(t1 : java.util.Date, t2 : java.util.Date) : Option[Interval] = {
          val tn1 = t1.getTime
          val tn2 = t2.getTime
          if (tn1 <= tn2) Some(new Interval(tn1, tn2)) else None  
      }
      
      def makeWindows(wins : List[List[java.util.Date]]) : Option[ List[ Option[Interval]] ] = wins match {
        case Nil => None
        case _ => 
          Some( wins.map { win => if (win.length != 2) None else makeInterval(win(0), win(1)) } )
      }

      val t0 = windows.map { case (k, wins) => (k, makeWindows(wins)  ) }
      val t1 = t0.collect { case (k,Some(v)) => (k,v.flatten) }
      // val tx = t0.map { case (k, Some(wins)) => (k, wins.flatten  ) } // not complete
      t1
    }
    
    val w4 = windowToIntervals(w3)
    println(w4)
    
    val windowFile = label_dir / "combined_windows.json"
    // TODO val windows = readWindows(windowFile)
    // allDataFiles().map { data => data.map { x => runExp(x, windows) } }
    
  }
  
  
}