package pt.inescn.experiments.bosch

/**
 * sbt "run-main pt.inescn.experiments.bosch.TableSawExp"
 * sbt -mem 6144 "run-main pt.inescn.experiments.bosch.TableSawExp"        // ok (987 sec)
 * sbt -mem 7168 "run-main pt.inescn.experiments.bosch.TableSawExp"        // ok (243 sec)
 * sbt -mem 8192 "run-main pt.inescn.experiments.bosch.TableSawExp"        // ok (245 sec)
 * sbt -mem 12288 "run-main pt.inescn.experiments.bosch.TableSawExp"      // ok (254 sec)
 * 
 */
object TableSawExp {
  def main( args: Array[ String ] ) {

    import pt.inescn.utils.Utils._
    import com.github.lwhite1.tablesaw.api.Table

    val fileName = "/home/hmf/Desktop/bosch/Anonymized_Fuel_System.csv"
    //val fileName = "/home/hmf/Desktop/bosch/short_1012031.csv"
    //val fileName = "/home/hmf/Desktop/bosch/short_500000.csv"
    //val fileName = "/home/hmf/Desktop/bosch/short_500000_b.csv" // no header
    
    val dt: Table = time { Table.createFromCsv( fileName ) }
    //val dt: Table = time { Table.createFromCsv( fileName, false ) }  // did not work
    
    import collection.JavaConverters._
    
    println("Standard table")
    println(dt.columnNames.asScala.mkString("<",",",">") )
    println(dt.shape )
    println(dt.structure.print )
    println(dt.first(3).print())
    
    println("Get/set the nominals")
    import com.github.lwhite1.tablesaw.api.QueryHelper.column
    val nominals = dt.structure().selectWhere(column("Column Type").isEqualTo("SHORT_INT"))
    println(nominals.print )
    
    val nominalFeaturesCols = Set(11, 19, 45, 71, 133, 134, 145, 146, 147, 153, 156, 159, 161, 166) 
    
    // Use SAW format for faster loading
    // Need to run this with 12G instead of 7G otherwise GC error
    val dbName : String  = time { dt.save("/home/hmf/Desktop/bosch") }
    println(s"dbName = $dbName")
    val dts : Table  = time { Table.readTable(dbName) }

    def nominalInfo(colName : String) = {
        val col = dts.column(colName)
        val u = col.unique
        val sz = u.size()
        val miss = col.countMissing() 
        //val out = u.print
        val out = u.toDoubleArray().mkString("<",",",">")
        println(s"$colName: missing($miss) ; countUnique($sz)")
        println(s"Unique values : ${u.print}")
    }
    val colName1 = "Col11"
    nominalInfo(colName1)
    
    val colName2 = "Col19"
    nominalInfo(colName2)

  }
}