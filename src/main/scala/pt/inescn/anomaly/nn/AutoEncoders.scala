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
  def main( args: Array[ String ] ) {

    //println( allDataFiles().mkString(",") )
    //println( allLabelFiles().mkString(",") )

    case class OneDate( t1: java.util.Date )

    import pt.inescn.utils.NABUtils._
    import org.json4s._
    import org.json4s.native.JsonMethods._

    //import scala.io.Source 
    import better.files._
    import java.io.{ File => JFile }

    val windowFile = label_dir / "combined_windows.json"
    // TODO val windows = readWindows(windowFile)
    // allDataFiles().map { data => data.map { x => runExp(x, windows) } }

  }

}