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

    import pt.inescn.utils.TableSawUtils._

    // TODO: place in test harness

    def assertOnNearZeroCheck( nzc: NearZeroCheck,
                               freqRatio: Double, badFreqRatio: Boolean,
                               uniqueRatio: Double, badUniqueRatio: Boolean,
                               isConstant: Boolean ) = {
      assert( approxEqual( nzc.frequency_ratio, freqRatio ) )
      assert( nzc.bad_freq_ratio == badFreqRatio )
      assert( approxEqual( nzc.unique_val_ratio, uniqueRatio ) )
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
    addColumns( dt, c1, c2, c3 )

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

    val nzt1 = nearZeros( dt )
    println( nzt1.print )

    // write.table(mdrrDescr, file="/home/hmf/Desktop/bosch/mdrrdesc.csv", sep=",", fileEncoding="UTF-8", quote=TRUE)
    // write.table(mdrrDescr, file="/home/hmf/Desktop/bosch/mdrrdesc.csv", sep=",", fileEncoding="UTF-8", quote=TRUE,eol="\r\n")
    // write.table(mdrrDescr, file="/home/hmf/Desktop/bosch/mdrrdesc.csv", sep=",", fileEncoding="UTF-8", quote=TRUE,eol="\r\n", row.names = FALSE)
    // names(mdrrDescr)[names(mdrrDescr) == "w"] <- "w_"

    import com.github.lwhite1.tablesaw.io.csv.CsvReader

    // Lest use https://topepo.github.io/caret/pre-processing.html t check the near zero calculations
    // It uses the mdrrdesc data as an example - we will use that to check the results

    // When reading the file, type inference fails. Sampling fails. Sampling of 1st row allways occurs 
    // We used a smaller file to fill in the value of the first row of the column generating the error.
    // We keep doing this until inference succeeds. 
    val dtt = CsvReader.detectedColumnTypes( "/home/hmf/Desktop/bosch/mdrrdesc_b.csv", true, ',' )
    val tps = toColumnTypes( dtt )
    //println(tps.mkString("{",",","}"))

    // We can now attempt to read the full file - using the types identified above
    val dt1: Table = time { Table.createFromCsv( tps.toArray, "/home/hmf/Desktop/bosch/mdrrdesc.csv" ) }
    println( dt1.first( 3 ).print )
    println( dt1.structure.first( 5 ).print )

    // Now calculate and check for near zero columns
    import com.github.lwhite1.tablesaw.api.QueryHelper.anyOf
    import com.github.lwhite1.tablesaw.api.QueryHelper.column

    val nzt2 = nearZeros( dt1 )
    println( nzt2.first( 5 ).print )
    val filtered = nzt2.selectWhere(
      anyOf(
        column( "Column" ).isEqualTo( "nTB" ),
        column( "Column" ).isEqualTo( "nBR" ),
        column( "Column" ).isEqualTo( "nI" ),
        column( "Column" ).isEqualTo( "nR03" ),
        column( "Column" ).isEqualTo( "nR08" ),
        column( "Column" ).isEqualTo( "nR11" ),
        column( "Column" ).isEqualTo( "nR12" ),
        column( "Column" ).isEqualTo( "D.Dr03" ),
        column( "Column" ).isEqualTo( "D.Dr07" ),
        column( "Column" ).isEqualTo( "D.Dr08" ) ) );
    println( filtered.print() )
    val r = filtered.floatColumn( "freqRatio" )
    val z = r.asScala.map( _.toDouble ).zip( List( 23.0, 131.0, 527.0, 527.0, 527.0, 21.782608, 57.666668, 527.0, 123.5, 527.0 ) )
    //println(z.mkString(","))
    //println(z.forall( p => p._1 == p._2))
    assert( z.zipWithIndex.forall{ case ( ( a, b ), i ) => aproxEqualShow( i, a, b ) } )

    val freqRatiosC1 = filtered.floatColumn( "freqRatio" )
    val freqRatios1 = freqRatiosC1.asScala.map( _.toDouble ).zip( List( 23.0, 131.0, 527.0, 527.0, 527.0, 21.782608, 57.666668, 527.0, 123.5, 527.0 ) )

    val uniqueValRatioC1 = filtered.floatColumn( "uniqueValRatio" )
    val l2 = List( 0.3787879, 0.3787879, 0.3787879, 0.3787879, 0.3787879, 0.5681818, 0.3787879, 0.3787879, 5.8712121, 0.3787879 ).map( _ / 100.0 )
    val uniqueValRatio1 = uniqueValRatioC1.asScala.map( _.toDouble ).zip( l2 )

    val nzv1 = filtered.booleanColumn( "badFreqRatio" ).toIntArray
      .zip( filtered.booleanColumn( "badUniqueValRatio" ).toIntArray )
      .map( x => ( x._1 > 0 ) || ( x._2 > 0 ) )
      .toIterable
    val nzvc1 = nzv1.zip( List( true, true, true, true, true, true, true, true, true, true ) )

    val const1 = filtered.booleanColumn( "isConstant" ).toIntArray.map( _ > 0 ).toIterable
    val constc1 = const1.zip( List( false, false, false, false, false, false, false, false, false, false ) )

    val chks1 = Map( "freqRatio" -> freqRatios1, "uniqueValRatio" -> uniqueValRatio1 )
    checkFloatColumnValues( chks1 )
    val chks2 = Map( "nzv" -> nzvc1, "zeroVar" -> constc1 )
    checkBooleanColumnValues( chks2 )

    // Now calculate and check for near zero columns

    // correlations/covariances and significance levels for pearson and spearman correlations.
    //  Spearman correlations are the Pearson linear correlations computed on the ranks of non-missing elements, using midranks for ties. 
    // polychoric correlation
    // heterogeneous correlations in one matrix
    // pearson (numeric-numeric),
    // polyserial (numeric-ordinal),
    // and polychoric (ordinal-ordinal) 
    // partial correlations
    import smile.math.Math

    // https://github.com/haifengl/smile/blob/355198c504f1c45652542da6580a3041799cb0f8/math/src/test/java/smile/stat/hypothesis/TTestTest.java
    val x = Array( 44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1 )
    val y = Array( 2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8 )

    val cor1 = Math.cor( x, y )
    //val c1 = Math.cor(x$1, x$2)

    // Calculate Pearson's correlation
    val ct1 = dt1.column( "IAC" )
    val cti1 = dt1.columnIndex( "IAC" )
    val ct2 = dt1.column( "TIC0" )
    val cti2 = dt1.columnIndex( "TIC0" )
    val ctp1 = ct1.`type`
    val ctp2 = ct2.`type`
    assert( isNonNumeric( ctp1 ) == false )
    assert( isNonNumeric( ctp2 ) == false )
    val corr1 = applyColumns( Math.cor )( dt1, cti1, ctp1, cti2, ctp2 )
    println( s"corr = ${corr1}" )
    assert( approxEqual( corr1, 1.0 ) )

    // check all pairs of columns for correlation

    val t1 = time { combColumns( Math.cor )( dt1 ) }
    //println( t1.mkString( "<<", ",", ">>" ) )
    println( s"Calculated ${t1.size} pairs o correlations" )
    val t1cc = dt1.columnCount()
    //println(t1cc * (t1cc -1) / 2)
    assert( t1cc * ( t1cc - 1 ) / 2 == t1.size )

    val t2 = time { findCorrelation( Math.cor, _ >= 0.75 )( dt1 ) }
    // println( t2.mkString( "<<", ",", ">>" ) )
    println( s"Found ${t2.size} pairs o significant correlations" )
    assert( t1.size >= t2.size )

    
    def chk_if_dep(v: (Int, Int,_)) = {
          if ( ( v._1 ) == 174 || ( v._2 == 174 ) ) println( s"Found correlation with dependent variable: (${v._1}, ${v._2})" )
    }

    val dbName = "/home/hmf/Desktop/bosch/Anonymized_Fuel_System.csv.saw"
    val dts: Table = time { Table.readTable( dbName ) }
    
    
    // 7 minutes
   /*
      BOSCH 2
      Elapsed time: 6.145489914sec
      Elapsed time: 417.587555629sec
      Found 377 pairs o significant correlations from a total of 15225
      Elapsed time: 0.004895337sec
      Found 271 components of significantly correlated features from a total of 377 pairs of correlated features
      Found the following 25 components of significantly correlated features
      {0 -> Set(69, 0, 5, 10, 52, 14, 157, 57, 6, 173, 13, 2, 166, 148, 149, 22, 59, 144, 49, 7, 3, 150, 50, 143, 26, 158, 8, 58, 51),170 -> Set(170, 171),20 -> Set(20, 55, 56),78 -> Set(78, 85, 93),164 -> Set(164, 165),61 -> Set(61, 65),21 -> Set(21, 151, 156),53 -> Set(53, 54),32 -> Set(32, 36, 40, 44),17 -> Set(17, 169),27 -> Set(27, 28),71 -> Set(71, 145, 146, 147),12 -> Set(12, 47, 141),91 -> Set(91, 114),135 -> Set(135, 137),80 -> Set(88, 115, 120, 37, 110, 125, 106, 121, 132, 89, 116, 117, 33, 97, 109, 124, 96, 129, 41, 128, 105, 118, 81, 39, 98, 103, 80, 35, 112, 123, 127, 31, 43, 104, 119, 82, 126, 131, 90, 111, 122),18 -> Set(18, 46),72 -> Set(72, 73),87 -> Set(87, 95),139 -> Set(139, 140, 172),23 -> Set(23, 24, 154),30 -> Set(30, 34, 38, 42),19 -> Set(19, 167, 168),15 -> Set(15, 48, 142, 16),62 -> Set(62, 66)}
      Found a total of 131 correlated features
    */
    println( "BOSCH 2: Pearson" )
    report_correlation(Math.cor, Math.abs(_) >= 0.75)( chk_if_dep )(dts)
    // 43 minutes
    println( "BOSCH 3: Spearman" )
    report_correlation(Math.spearman, Math.abs(_) >= 0.75)( chk_if_dep )(dts)
    println( "BOSCH 3: Kendall" )
    //report_correlation(Math.kendall,Math.abs(_) >= 0.75)( chk_if_dep )(dts)
    
    // TODO: for each group check if the distribution is a multivariate normal distribution

    // TODO: create a Smile utilities object (implicit?)

    // TODO: ran correlation - Spearman and Kendall
    // TODO: other correlates in https://en.wikipedia.org/wiki/Correlation_and_dependence
    // TODO:  	JensenShannonDivergence
    // TODO: KullbackLeiblerDivergence
    
    // TODO: check co-linearity

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
