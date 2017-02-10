package pt.inescn.experiments.bosch

import javax.xml.ws.Response

/**
 * For full file
 * sbt "run-main pt.inescn.experiments.bosch.Exp1"
 * 
 * env JAVA_OPTS="-Xmx512m" sbt "run-main pt.inescn.experiments.bosch.Exp1"
 * sbt -mem 2048 "run-main pt.inescn.experiments.bosch.Exp1"
 * sbt -mem 4096 "run-main pt.inescn.experiments.bosch.Exp1"        // fails
 * sbt -mem 5120 "run-main pt.inescn.experiments.bosch.Exp1"        // fails
 * sbt -mem 6144 "run-main pt.inescn.experiments.bosch.Exp1"        // fails
 * sbt -mem 7168 "run-main pt.inescn.experiments.bosch.Exp1"        // ok (112 sec)
 * sbt -mem 8192 "run-main pt.inescn.experiments.bosch.Exp1"        // ok (92 sec)
 * sbt -mem 12288 "run-main pt.inescn.experiments.bosch.Exp1"      // ok (75 sec)
 * 
 * env JAVA_OPTS="-Xmx10G" sbt "run-main pt.inescn.experiments.bosch.Exp1"
 * 
 * @see http://stackoverflow.com/questions/3868863/how-to-specify-jvm-maximum-heap-size-xmx-for-running-an-application-with-run
 * 
 * wc -l Anonymized_Fuel_System.csv
 *   4709655 Anonymized_Fuel_System.csv
 * head -1012031 Anonymized_Fuel_System.csv >> short_1012031.csv
 * wc -l short_1012031.csv 
 *   1012031 short_1012031.csv
 * head -500000 Anonymized_Fuel_System.csv >> short_500000.csv
 *   
 *  file -bi Anonymized_Fuel_System.csv
 *  text/plain; charset=us-ascii
 *  file -bi short_500000.csv
 *  text/plain; charset=us-ascii 
 *  iconv -f ascii -t utf-8 short_500000.csv > short_500000_b.csv
 *  file -bi short_500000_b.csv
 *  text/plain; charset=us-ascii 
 * 
 * 
 * TODO: try Breeze http://stackoverflow.com/questions/14885553/read-a-matrix-from-a-file-in-scala-breeze
 * https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet
 * TODO: Try http://bid2.berkeley.edu/bid-data-project/
 * http://neuroph.sourceforge.net/index.html
 * https://elki-project.github.io/
 * 
 * @see https://github.com/josephmisiti/awesome-machine-learning
 * 
 * TODO: https://saddle.github.io/
 * https://github.com/martincooper/scala-datatable
 * https://github.com/tixxit/framian
 * https://darrenjw.wordpress.com/2015/08/21/data-frames-and-tables-in-scala/
 * https://github.com/scalanlp/breeze
 * https://github.com/scalanlp/breeze/issues/470
 * http://factorie.cs.umass.edu/
 * http://ddf.io/
 * 
 * Java 
 * @see http://stackoverflow.com/questions/20540831/java-object-analogue-to-r-data-frame
 * https://github.com/cardillo/joinery
 * https://github.com/netzwerg/paleo
 * https://github.com/lwhite1/tablesaw
 * 
 * http://machinelearningmastery.com/java-machine-learning/
 * 
 * Package smile.feature
 * Feature generation, normalization and selection.
 * 
 */
object Exp1 {

  import pt.inescn.utils.Utils.time
  
  /**
   * @see https://github.com/haifengl/smile/wiki/Tutorial:-A-Gentle-Introduction-to-Smile
   * @see https://xyclade.github.io/MachineLearning/
   */
  def main( args: Array[ String ] ) {
    //val file = "/home/hmf/Desktop/bosch/Anonymized_Fuel_System.csv"
    //val fileName = "/home/hmf/Desktop/bosch/short_1012031.csv"
    //val fileName = "/home/hmf/Desktop/bosch/short_500000.csv"
    val fileName = "/home/hmf/Desktop/bosch/short_500000_b.csv" // no header

    import smile.read._
    import smile.data.AttributeDataset
    import smile.data.NominalAttribute
  
    // csv(file: String, response: Option[(Attribute, Int)] = None, comment: String = "%", missing: String = "?", header: Boolean = false, rowNames: Boolean = false): AttributeDataset
    val targetIdx = 174
    //val target = Some((new NominalAttribute("class"), targetIdx))
    //val ds: AttributeDataset = time { csv( file = fileName, header = true, response = target) }
    
    import smile.data.parser.DelimitedTextParser
    import java.io.File
    import smile.data.NumericAttribute
    
    //def colName(x : Int) = '"' +  "Col"+x + '"'  // Original header wraps column names with quotes
    def colName(x : Int) = "Col"+x
    val colNames = (0 to 173) map { x => colName(x) }
    val colAttributes1 = colNames map { x => new NumericAttribute(x) }
    println(colNames.size)
    val nominalFeaturesCols = Set(11, 19, 45, 71, 133, 134, 145, 146, 147, 153, 156, 159, 161, 166) 
    val nominalFeaturesIndxs = nominalFeaturesCols map colName
    val colAttributes2 = colAttributes1 map { x => 
                     if (nominalFeaturesIndxs.contains(x.getName) ) new NominalAttribute(x.getName) else x  } 
    val colAttributes = colAttributes2.toArray
    
    val file = new File(fileName)
    val parser : DelimitedTextParser = new DelimitedTextParser();
    val ds: AttributeDataset =  parser.setResponseIndex(new NominalAttribute( colName(targetIdx) ), targetIdx)
                                              .setDelimiter(",")
                                              //.setCommentStartWith("#")
                                              .parse(colAttributes, file);
    // If we have no header
    // parser.setColumnNames(true) 
    
    // smile.PimpedDataset
    time { println(s"size = ${ds.size()}") }
    time { println(s"attributes = ${ds.attributes().mkString("<",",",">")}") }
    time { println(s"colnames = ${ds.colnames.mkString("<",",",">")}") }
    time { println(s"response = ${ds.response()}") }
    time { println(s"summary = ${ds.summary}") }

    // Irrespective of the column size, the ds.toArray will always place all columns in the matrix
    //val x : double[][] = ds.toArray(new double[ds.size()][]);
    val x = ds.toArray( Array.ofDim[Double](ds.size(), ds.colnames.size) );
    //val x = ds.toArray( Array.ofDim[Double](ds.size, nominalFeaturesIndxs.size) );
    
    println("??????????????")
    println(x(0).mkString(","))
    
    def dropColumn(mat: Array[Array[Double] ] , col: Int) = {
      /*for (row <- x.indices)
        x(row) = x(row).drop(1)*/
       x.transform(_.drop(1))
    }
    
    def dropColumns(mat: Array[Array[Double] ] , cols: List[Int]) = {
      val row_len = mat(0).length
      // second of tuple is index-offset
      val offset = cols.zipWithIndex
      offset.foreach { case (i,o) => dropColumn(mat, i-o) }
    }
    
    println("!!!!!!!!!!!!!!!!!!!!!!!!!!")
    //dropColumn(x , 0)
    //dropColumns(x, nominalFeaturesCols.toList)
    val all = (0 to 173).toSet
    val keep = all.diff(nominalFeaturesCols)
    dropColumns(x, keep.toList)
    println(x(0).mkString(","))
    
    // One-hot-encoding
    // https://haifengl.github.io/smile/api/java/smile/feature/Nominal2Binary.html
    // http://programtalk.com/vs/smile/core/src/test/java/smile/feature/FeatureSetTest.java/
    // http://programtalk.com/vs/?source=smile/core/src/test/java/smile/feature/FeatureSetTest.java
    import smile.feature.Nominal2Binary
    println(s"Nominal attribute indexes = ${nominalFeaturesIndxs.mkString("<",",",">")}")
    val nominalAttributes = ds.attributes().filter { x => nominalFeaturesIndxs.contains(x.getName)  }
    println(s"Nominal attributes = ${nominalAttributes.mkString("<",",",">")}")
    val oneHotEncoding = new Nominal2Binary(nominalAttributes)
    val nominalEncodings = oneHotEncoding.attributes()
    println(s"Nominal encodings = ${nominalEncodings.mkString("<",",",">")}")
    val c11_1 = oneHotEncoding.f( x(11), 0) // 1..10
    
    // How do we replace the columns?
    
    // https://haifengl.github.io/smile/api/java/smile/feature/NumericAttributeFeature.html
  }
}