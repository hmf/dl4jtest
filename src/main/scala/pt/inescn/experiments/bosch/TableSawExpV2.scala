package pt.inescn.experiments.bosch

/**
 * sbt "run-main pt.inescn.experiments.bosch.TableSawExpV2"
 * sbt -mem 6144 "run-main pt.inescn.experiments.bosch.TableSawExpV2"        // ok (987 sec)
 * sbt -mem 7168 "run-main pt.inescn.experiments.bosch.TableSawExpV2"        // ok (243 sec)
 * sbt -mem 8192 "run-main pt.inescn.experiments.bosch.TableSawExpV2"        // ok (245 sec)
 * sbt -mem 12288 "run-main pt.inescn.experiments.bosch.TableSawExpV2"      // ok (254 sec)
 *
 * @see https://darrenjw.wordpress.com/2015/08/21/data-frames-and-tables-in-scala/
 * @see http://stackoverflow.com/questions/20540831/java-object-analogue-to-r-data-frame
 * @see https://github.com/lwhite1/tablesaw
 * @see https://github.com/cardillo/joinery
 * @see https://github.com/netzwerg/paleo
 * @see http://stackoverflow.com/questions/10462507/any-good-library-to-read-and-write-csv-files
 *
 *  *
 * TODO:
 * Zero variance
 * Near-zero variance
 * Package smile.feature
 * Feature generation, normalization and selection.
 *
 */
object TableSawExpV2 {
  def main( args: Array[ String ] ) {

    import pt.inescn.utils.Utils._
    import com.github.lwhite1.tablesaw.api.Table
    /*
    val fileName = "/home/hmf/Desktop/bosch/Anonymized_Fuel_System.csv"
    //val fileName = "/home/hmf/Desktop/bosch/short_1012031.csv"
    //val fileName = "/home/hmf/Desktop/bosch/short_500000.csv"
    //val fileName = "/home/hmf/Desktop/bosch/short_500000_b.csv" // no header
    
    //val dt: Table = time { Table.createFromCsv( fileName ) }
    //val dt: Table = time { Table.createFromCsv( fileName, false ) }  // did not work
    
    // Use SAW format for faster loading
    // Need to run this with 12G instead of 7G otherwise GC error
    //val dbName : String  = time { dt.save("/home/hmf/Desktop/bosch/") }
    val dbName = "/home/hmf/Desktop/bosch/Anonymized_Fuel_System.csv.saw"
    val dts : Table  = time { Table.readTable(dbName) }
    
    import collection.JavaConverters._
    
    println("Standard table")
    println(dts.columnNames.asScala.mkString("<",",",">") )
    println(dts.shape )
    println(dts.structure.print )
    println(dts.first(3).print())
    
    // Check if any column has missing data
    import com.github.lwhite1.tablesaw.api.QueryHelper.column
    //def NA(colName : String) = column(colName).isMissing()
    def NA(colName : String) = column(colName).isNotMissing()
    // Create a filter for each column
    val naSFilters = dts.columnNames().asScala map { x => NA(x) }
    // Make sure its a valid Java collection
    val naFilters = naSFilters.asJavaCollection
    // Requires 8G
    // For any column select rows that have missing values
    import com.github.lwhite1.tablesaw.api.QueryHelper.anyOf
    val nas = dts.selectWhere( anyOf(naFilters) )
    // Testin - not missing
    // Requires 8G
    //import com.github.lwhite1.tablesaw.api.QueryHelper.allOf
    //val nas = dts.selectWhere( allOf(naFilters) )
    println(s"Found ${nas.rowCount()} rows with missing data")
    
    println("Get/set the nominals")
    val nominals = dts.structure().selectWhere(column("Column Type").isEqualTo("SHORT_INT"))
    println(nominals.print )
    
    val nominalFeaturesCols = List(11, 19, 45, 71, 133, 134, 145, 146, 147, 153, 156, 159, 161, 166) 
    
    def nominalInfo(dts : Table, colName : String) = {
        val col = dts.column(colName)
        val u = col.unique
        val sz = u.size()
        val miss = col.countMissing() 
        //val out = u.print
        val out = u.toDoubleArray().mkString("<",",",">")
        println(s"$colName (${col.size}): missing($miss) ; countUnique($sz)")
        val numRows = 10
        println(s"Unique values (first $numRows) : ${u.first(numRows).print}")
    }
    
    val noms = nominals.categoryColumn("Column Name")
    println(s"Nominals : type(${noms.`type`})")
    //time { noms.asScala.foreach { colName =>  nominalInfo(dts, colName) } }
    
    val setNoms = noms.asScala.toSet
    def colName(x : Int) = "Col"+x
    val nominalFeaturesIndxs = nominalFeaturesCols map colName 
    val unexpectedNoms1 = setNoms.diff(nominalFeaturesIndxs.toSet)
    val unexpectedNoms2 = nominalFeaturesIndxs.toSet.diff(setNoms)
    println(unexpectedNoms1.mkString("{", ",", "}"))
    println(unexpectedNoms2.mkString("{", ",", "}"))

    // These do not seem to be nominal features. Error?
    println("Missing nominals:")
    def veryFewValues(dts : Table, colName : String, limit : Int) = {
        val col = dts.column(colName)
        val u = col.unique
        val sz = u.size()
        if (sz <= limit) true else false
     }
    unexpectedNoms2.foreach { colName =>  nominalInfo(dts, colName)}
    
    val limit = 30
    println(s"Possibly missing nominals (max unique values, inclusive = $limit):")
    unexpectedNoms1.filter { colName => veryFewValues(dts, colName, limit) }
                              .foreach { colName =>  nominalInfo(dts, colName)}
*/

    import com.github.lwhite1.tablesaw.api.ColumnType
    import com.github.lwhite1.tablesaw.columns.Column
    import com.github.lwhite1.tablesaw.api.IntColumn
    import com.github.lwhite1.tablesaw.api.BooleanColumn
    import com.github.lwhite1.tablesaw.api.FloatColumn
    import com.github.lwhite1.tablesaw.api.CategoryColumn

    import collection.JavaConverters._

    case class NearZeroCheck( frequency_ratio: Double, bad_freq_ratio: Boolean,
                              unique_val_ratio: Double, bad_unique_val_ratio: Boolean,
                              isConstant : Boolean)

                              
    def calcRatios[ V ]( l : Iterable[V], uniqueCut: Double, freqCut: Double ) = {
      val r = l.groupBy( identity ).mapValues( _.size )
      val s = r.toList.sortBy { x => - x._2 }
      val ratio = if (s.size >= 2)  { 
          val top = s( 0 )._2
          val top_1 = s( 1 )._2
          top.toDouble / top_1.toDouble
        } else {
          Double.NaN
        }
      val bad_ratio = if ( ratio > freqCut ) true else false
      val unique_ratio = r.size / l.size.toDouble
      val bad_unique_ratio = if ( unique_ratio < uniqueCut ) true else false
      NearZeroCheck( ratio, bad_ratio, unique_ratio, bad_unique_ratio, r.size == 1 )
    }

    /*
      * The check for near zero variance verifies the following conditions:
      * 1) few unique values relative to the number of samples 
      * 2) large ratio of the frequency of the most common value to the frequency of the second most 
      *     common value (near-zero variance predictors).
      * 
      * More specifcally:
      * - The frequency of the most prevalent value over the second most frequent value (called the 
      *   “frequency ratio’’), which would be near one for well-behaved predictors and very large for 
      *   highly-unbalanced data
      *  
      * - The “percent of unique values’’ is the number of unique values divided by the total number of 
      *   samples (times 100) that approaches zero as the granularity of the data increases
      * 
      * By default, a predictor is classified as near-zero variance if the percentage of unique values 
      * in the samples is less than {10\%} and when the frequency ratio mentioned above is greater 
      * than 19 (95/5). 
      */
    def nearZero[ K, V ]( t: Table, colName: String, uniqueCut: Double = 0.1, freqCut: Double = 19 ) = {
      val col = t.column( colName )
      val meta = col.columnMetadata()
      val tp = meta.getType
      tp match {
        case ColumnType.BOOLEAN =>
          val c = t.booleanColumn( colName )
          val l = c.toIntArray
          calcRatios( l, uniqueCut, freqCut )
        case ColumnType.CATEGORY =>
          val c = t.categoryColumn( colName )
          val l = c.asScala
          calcRatios( l, uniqueCut, freqCut )
        case ColumnType.FLOAT =>
          val c = t.floatColumn( colName )
          // TODO: val stats = c.stats() ?????
          val l = c.asScala
          calcRatios( l, uniqueCut, freqCut )
        case ColumnType.SHORT_INT =>
          val c = t.shortColumn( colName )
          val l = c.asScala
          calcRatios( l, uniqueCut, freqCut )
        case ColumnType.INTEGER =>
          val c = t.intColumn( colName )
          val l = c.asScala
          calcRatios( l, uniqueCut, freqCut )
        case ColumnType.LONG_INT =>
          val c = t.longColumn( colName )
          val l = c.asScala
          calcRatios( l, uniqueCut, freqCut )
        case ColumnType.LOCAL_DATE =>
          val c = t.dateColumn( colName )
          val l = c.asScala
          calcRatios( l, uniqueCut, freqCut )
        case ColumnType.LOCAL_DATE_TIME =>
          val c = t.dateTimeColumn( colName )
          val l = c.asScala
          calcRatios( l, uniqueCut, freqCut )
        case ColumnType.LOCAL_TIME =>
          val c = t.timeColumn( colName )
          val l = c.asScala
          calcRatios( l, uniqueCut, freqCut )
        case ColumnType.SKIP =>
          NearZeroCheck( Double.NaN, true, Double.NaN, true, true )
      }
    }

    def nearZeros( t: Table, uniqueCut: Double = 0.1, freqCut: Double = 19 ) = {
      val cols = t.columnNames.asScala
      val nzc = cols.map { colName => nearZero( t, colName, uniqueCut, freqCut ) }
      val dt = Table.create( s"Near Zero check for ${t.name}" )
      val col0 = CategoryColumn.create( "Column" )
      val col1 = FloatColumn.create( "freqRatio" )
      val col2 = BooleanColumn.create( "badFreqRatio" )
      val col3 = FloatColumn.create( "uniqueValRatio" )
      val col4 = BooleanColumn.create( "badUniqueValRatio" )
      val col5 = BooleanColumn.create( "isConstant" )
      val nt = cols.foreach { 
        colName =>
          val nzc = nearZero( t, colName, uniqueCut, freqCut )
          col0.add( colName)
          col1.add( nzc.frequency_ratio)
          col2.add(nzc.bad_freq_ratio)
          col3.add(nzc.unique_val_ratio)
          col4.add(nzc.bad_unique_val_ratio)
          col5.add(nzc.isConstant)
      }
      addColumn(dt, col0) // why cannot we not add tis too via the varargs?
      addColumns(dt, col1, col2, col3, col4, col5)
      dt
    }

    
    def assertOnNearZeroCheck( nzc: NearZeroCheck, 
        freqRatio : Double, badFreqRatio : Boolean, 
        uniqueRatio : Double, badUniqueRatio : Boolean,
        isConstant : Boolean ) = {
      assert( aproxEqual( nzc.frequency_ratio, freqRatio ) )
      assert( nzc.bad_freq_ratio == badFreqRatio )
      assert( aproxEqual( nzc.unique_val_ratio, uniqueRatio ) )
      assert( nzc.bad_unique_val_ratio == badUniqueRatio )
      assert( nzc.isConstant == isConstant )
    }

    val dt = Table.create( "test1" )
    val c1 = createIntColumn( "col1", List( 1, 1, 2, 1, 3, 2, 4 ) )
    val c2 = createIntColumn( "col2", List( 1, 2, 3, 4, 5, 6, 7 ) )
    val c3 = createIntColumn( "col3", List( 1, 1, 1, 1, 1, 1, 1 ) )
    
    //val col1 = c1.asInstanceOf[ Column[ _ ] ]
    //val col2 = c2.asInstanceOf[ Column[ _ ] ]
    //dt.addColumn( col1, col2 )
    addColumns( dt, c1, c2, c3)

    val nzc1 = nearZero( dt, "col1" )
    println( nzc1 )
    assertOnNearZeroCheck( nzc1, 1.5, false, 0.571429, false, false )

    val nzc2 = nearZero( dt, "col2" )
    println( nzc2 )
    assertOnNearZeroCheck( nzc2, 1.0, false, 1.0, false, false )

    val nzc3 = nearZero( dt, "col3" )
    println( nzc3 )
    //assertOnNearZeroCheck( nzc3, Double.NaN, false, 0.14285714285714285, false, true )
    assertOnNearZeroCheck( nzc3, Double.NaN, false, 0.142857, false, true )
    
    val nzt1 = nearZeros(dt)
    println(nzt1.print)
    
    // write.table(mdrrDescr, file="/home/hmf/Desktop/bosch/mdrrdesc.csv", sep=",", fileEncoding="UTF-8", quote=TRUE)
    // write.table(mdrrDescr, file="/home/hmf/Desktop/bosch/mdrrdesc.csv", sep=",", fileEncoding="UTF-8", quote=TRUE,eol="\r\n")
    // write.table(mdrrDescr, file="/home/hmf/Desktop/bosch/mdrrdesc.csv", sep=",", fileEncoding="UTF-8", quote=TRUE,eol="\r\n", row.names = FALSE)
    // names(mdrrDescr)[names(mdrrDescr) == "w"] <- "w_"
    
    import com.github.lwhite1.tablesaw.io.csv.CsvReader
    
    def toColumnType(s : String) : ColumnType = {
      s match {
        case "BOOLEAN" => ColumnType.BOOLEAN
        case "CATEGORY" => ColumnType.CATEGORY
        case "FLOAT" => ColumnType.FLOAT
        case  "SHORT_INT" => ColumnType.SHORT_INT
        case "INTEGER" => ColumnType.INTEGER
        case "LONG_INT" => ColumnType.LONG_INT
        case "LOCAL_DATE" => ColumnType.LOCAL_DATE
        case "LOCAL_DATE_TIME" => ColumnType.LOCAL_DATE_TIME
        case "LOCAL_TIME" => ColumnType.LOCAL_TIME
        case "SKIP" => ColumnType.SKIP    
      }
    }
    
   def toColumnTypes(t : Table) : Iterable[ColumnType] = {
      val typeCol  = t.column("Column Type")
      val typeColIndex = t.columnIndex(typeCol)
      val tps = t.categoryColumn(typeColIndex)
      val types = tps.asScala.map { x => toColumnType(x) }
      types
    }
    
   // Lest use https://topepo.github.io/caret/pre-processing.html t check the near zero calculations
   // It uses the mdrrdesc data as an example - we will use that to check the results
   
   // When reading the file, type inference fails. Sampling fails. Sampling of 1st row allways occurs 
   // We used a smaller file to fill in the value of the first row of the column generating the error.
   // We keep doing this until inference succeeds. 
    val dtt = CsvReader.detectedColumnTypes("/home/hmf/Desktop/bosch/mdrrdesc_b.csv",  true, ',')
    val tps = toColumnTypes(dtt) 
    //println(tps.mkString("{",",","}"))
 
    // We can now attempt to read the full file - using the types identified above
    val dt1: Table = time { Table.createFromCsv( tps.toArray, "/home/hmf/Desktop/bosch/mdrrdesc.csv" ) }
    println(dt1.first(3).print)
    println(dt1.structure.first(5).print)

    // Now calculate and check for near zero columns
    import com.github.lwhite1.tablesaw.api.QueryHelper.anyOf
    import com.github.lwhite1.tablesaw.api.QueryHelper.column

    /*
## nTB     23.00000     0.3787879   FALSE TRUE
## nBR    131.00000     0.3787879   FALSE TRUE
## nI     527.00000     0.3787879   FALSE TRUE
## nR03   527.00000     0.3787879   FALSE TRUE
## nR08   527.00000     0.3787879   FALSE TRUE
## nR11    21.78261     0.5681818   FALSE TRUE
## nR12    57.66667     0.3787879   FALSE TRUE
## D.Dr03 527.00000     0.3787879   FALSE TRUE
## D.Dr07 123.50000     5.8712121   FALSE TRUE
## D.Dr08 527.00000     0.3787879   FALSE TRUE
     */

    /*
##    nTB     23.00000     0.3787879   FALSE TRUE
## nBR    131.00000     0.3787879   FALSE TRUE
## nI     527.00000     0.3787879   FALSE TRUE
## nR03   527.00000     0.3787879   FALSE TRUE
## nR08   527.00000     0.3787879   FALSE TRUE
## nR11    21.78261     0.5681818   FALSE TRUE
## nR12    57.66667     0.3787879   FALSE TRUE
## D.Dr03 527.00000     0.3787879   FALSE TRUE
## D.Dr07 123.50000     5.8712121   FALSE TRUE
## D.Dr08
*/

  def checkColumnValues[T,U,V](m : Map[String, Iterable[(T,U)]], chkShow: (Int,T,U,V) => Boolean, eps: V) = {
      m.forall{ p =>  
        println(s"Checking ${p._1}")
        val t = p._2.zipWithIndex
        t.forall{ case ((a,b),i) => chkShow(i, a, b, eps) }
      }
    }
    
    def checkFloatColumnValues(m : Map[String, Iterable[(Double, Double)]]) = {
      assert(checkColumnValues[Double, Double, Double](m, aproxEqualShow, eps=0.000001))
    }

   def checkBooleanColumnValues(m : Map[String, Iterable[(Boolean, Boolean)]]) = {
      assert(checkColumnValues[Boolean, Boolean, Boolean](
          m, 
          { (i: Int, x:Boolean,y:Boolean,_) => val r = (x == y) ; if (!r) println(s"At $i expected $x but got $y") ; r }, 
          eps=true)
          )
    }

    val nzt2 = nearZeros(dt1)
    println(nzt2.first(5).print)
    val filtered = nzt2.selectWhere(
        anyOf(
            column("Column").isEqualTo("nTB"),
            column("Column").isEqualTo("nBR"),
            column("Column").isEqualTo("nI"),
            column("Column").isEqualTo("nR03"),
            column("Column").isEqualTo("nR08"),
            column("Column").isEqualTo("nR11"),
            column("Column").isEqualTo("nR12"),
            column("Column").isEqualTo("D.Dr03"),
            column("Column").isEqualTo("D.Dr07"),
            column("Column").isEqualTo("D.Dr08")
            )
        );    
    println(filtered.print())
    val r = filtered.floatColumn("freqRatio")
    val z = r.asScala.map(_.toDouble).zip(List(23.0, 131.0, 527.0, 527.0, 527.0, 21.782608, 57.666668, 527.0, 123.5, 527.0))
    //println(z.mkString(","))
    //println(z.forall( p => p._1 == p._2))
    assert(z.zipWithIndex.forall{ case ((a,b),i) => aproxEqualShow(i, a, b) } )
    
    val freqRatiosC1 = filtered.floatColumn("freqRatio")
    val freqRatios1 = freqRatiosC1.asScala.map(_.toDouble).zip(List(23.0, 131.0, 527.0, 527.0, 527.0, 21.782608, 57.666668, 527.0, 123.5, 527.0))

    val uniqueValRatioC1 = filtered.floatColumn("uniqueValRatio")
    val l2 = List(0.3787879, 0.3787879, 0.3787879, 0.3787879, 0.3787879, 0.5681818, 0.3787879, 0.3787879, 5.8712121, 0.3787879 ).map(_ / 100.0)
    val uniqueValRatio1 = uniqueValRatioC1.asScala.map(_.toDouble).zip(l2)
    
    val nzv1 = filtered.booleanColumn("badFreqRatio").toIntArray
                               .zip(filtered.booleanColumn("badUniqueValRatio").toIntArray)
                               .map( x =>  (x._1 > 0) || (x._2 > 0))
                               .toIterable
    val nzvc1 = nzv1.zip(List(true, true, true, true, true, true, true, true, true, true))

    
    val const1 = filtered.booleanColumn("isConstant").toIntArray.map( _ > 0).toIterable
    val constc1 = const1.zip(List(false, false, false, false, false, false, false, false, false, false))
    
    val chks1 = Map("freqRatio" -> freqRatios1, "uniqueValRatio" -> uniqueValRatio1)
    checkFloatColumnValues(chks1)
    val chks2 = Map("nzv" -> nzvc1, "zeroVar" -> constc1)
    checkBooleanColumnValues(chks2)
    
    // correlations/covariances and significance levels for pearson and spearman correlations.
    //  Spearman correlations are the Pearson linear correlations computed on the ranks of non-missing elements, using midranks for ties. 
    // polychoric correlation
    // heterogeneous correlations in one matrix
    // pearson (numeric-numeric),
    // polyserial (numeric-ordinal),
    // and polychoric (ordinal-ordinal) 
    // partial correlations
    import smile.math.Math
    
    //val c1 = Math.cor(x$1, x$2)
    
    /*
    // Example of using cross-tabs
    import com.github.lwhite1.tablesaw.reducing.CrossTab

    // Circumvent Scala's typing system
    val col1 = c1.asInstanceOf[ Column[ _ ] ]
    val col2 = c2.asInstanceOf[ Column[ _ ] ]
    //val col1 = c1.asInstanceOf[Column[AnyRef]]
    //val col1 = c1.asInstanceOf[Column[Any]]
    //val col1 = c1.asInstanceOf[Column[IntColumn]]
    dt.addColumn( col1, col2 )
    println( dt.print )

    //val xTab = CrossTab.columnPercents(dt)
    //val xTab = CrossTab.columnPercents(dt)
    //val xTab = CrossTab.xTabCount(dt, c1, c2)
    //val xTab = CrossTab.xTabCount(dt, col1: Column[_], col2 : Column[_])
    //val xTab = CrossTab.xTabCount(dt, col1: Column[IntColumn], col2 : Column[_])
    //val xTab = CrossTab.xTabCount(dt, c1: IntColumn, c2: IntColumn)
    import pt.inescn.scratchpad.utils.Utils

    // We need to use Java to circumvet Scala's typing system. 
    val xTab = Utils.xTabCountCols( dt, c1, c2 )
    println( xTab.print )

    val r1 = CrossTab.columnPercents( xTab )
    println( r1.print )
    // Seems to be broken, no tests
    val r2 = CrossTab.rowPercents( xTab )
    println( r2.print )
    */
  }
}