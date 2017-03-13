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
    import com.github.lwhite1.tablesaw.io.csv.CsvReader
    
    // We can now attempt to read the full file - using the types identified above
    val file = "/home/hmf/Desktop/bosch/mdrrdesc.csv"
    val header = true
    val delimiter = ','
    val skipSampling = true
    
     val ntps = CsvReader.detectColumnTypes(file, header, delimiter, skipSampling)
     println(ntps.mkString("{", ",", "}"))
    
    //val dt1: Table = time { Table.createFromCsv( ???, "/home/hmf/Desktop/bosch/mdrrdesc.csv" ) }
    //val dt1 : Table = time { Table.createFromCsv( "/home/hmf/Desktop/bosch/mdrrdesc.csv", header, delimiter, skipSampling ) }
    val dt1 : Table = time { Table.createFromCsv( file, header, delimiter, skipSampling ) }
    
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

    // write.table(mdrrDescr, file="/home/hmf/Desktop/bosch/mdrrdesc.csv", sep=",", fileEncoding="UTF-8", quote=TRUE)
    // write.table(mdrrDescr, file="/home/hmf/Desktop/bosch/mdrrdesc.csv", sep=",", fileEncoding="UTF-8", quote=TRUE,eol="\r\n")
    // write.table(mdrrDescr, file="/home/hmf/Desktop/bosch/mdrrdesc.csv", sep=",", fileEncoding="UTF-8", quote=TRUE,eol="\r\n", row.names = FALSE)
    // names(mdrrDescr)[names(mdrrDescr) == "w"] <- "w_"

    import com.github.lwhite1.tablesaw.io.csv.CsvReader

    /* TODO 
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
*/

    def chk_if_dep( v: ( Int, Int, _ ) ) = {
      if ( ( v._1 ) == 174 || ( v._2 == 174 ) ) println( s"Found correlation with dependent variable: (${v._1}, ${v._2})" )
    }

    val dbName = "/home/hmf/Desktop/bosch/Anonymized_Fuel_System.csv.saw"
    val dts: Table = time { Table.readTable( dbName ) }

    import smile.math.Math

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
    //report_correlation(Math.cor, Math.abs(_) >= 0.75)( chk_if_dep )(dts)
    // 43 minutes
    /*
    Elapsed time: 2291.405744404sec
    Found 15225 pairs o significant correlations from a total of 15225
    Found correlation with dependent variable: (0, 174)
    Found correlation with dependent variable: (1, 174)
    Found correlation with dependent variable: (2, 174)
    Found correlation with dependent variable: (3, 174)
    Found correlation with dependent variable: (4, 174)
    Found correlation with dependent variable: (5, 174)
    Found correlation with dependent variable: (6, 174)
    Found correlation with dependent variable: (7, 174)
    Found correlation with dependent variable: (8, 174)
    Found correlation with dependent variable: (9, 174)
    Found correlation with dependent variable: (10, 174)
    Found correlation with dependent variable: (11, 174)
    Found correlation with dependent variable: (12, 174)
    Found correlation with dependent variable: (13, 174)
    Found correlation with dependent variable: (14, 174)
    Found correlation with dependent variable: (15, 174)
    Found correlation with dependent variable: (16, 174)
    Found correlation with dependent variable: (17, 174)
    Found correlation with dependent variable: (18, 174)
    Found correlation with dependent variable: (19, 174)
    Found correlation with dependent variable: (20, 174)
    Found correlation with dependent variable: (21, 174)
    Found correlation with dependent variable: (22, 174)
    Found correlation with dependent variable: (23, 174)
    Found correlation with dependent variable: (24, 174)
    Found correlation with dependent variable: (25, 174)
    Found correlation with dependent variable: (26, 174)
    Found correlation with dependent variable: (27, 174)
    Found correlation with dependent variable: (28, 174)
    Found correlation with dependent variable: (29, 174)
    Found correlation with dependent variable: (30, 174)
    Found correlation with dependent variable: (31, 174)
    Found correlation with dependent variable: (32, 174)
    Found correlation with dependent variable: (33, 174)
    Found correlation with dependent variable: (34, 174)
    Found correlation with dependent variable: (35, 174)
    Found correlation with dependent variable: (36, 174)
    Found correlation with dependent variable: (37, 174)
    Found correlation with dependent variable: (38, 174)
    Found correlation with dependent variable: (39, 174)
    Found correlation with dependent variable: (40, 174)
    Found correlation with dependent variable: (41, 174)
    Found correlation with dependent variable: (42, 174)
    Found correlation with dependent variable: (43, 174)
    Found correlation with dependent variable: (44, 174)
    Found correlation with dependent variable: (45, 174)
    Found correlation with dependent variable: (46, 174)
    Found correlation with dependent variable: (47, 174)
    Found correlation with dependent variable: (48, 174)
    Found correlation with dependent variable: (49, 174)
    Found correlation with dependent variable: (50, 174)
    Found correlation with dependent variable: (51, 174)
    Found correlation with dependent variable: (52, 174)
    Found correlation with dependent variable: (53, 174)
    Found correlation with dependent variable: (54, 174)
    Found correlation with dependent variable: (55, 174)
    Found correlation with dependent variable: (56, 174)
    Found correlation with dependent variable: (57, 174)
    Found correlation with dependent variable: (58, 174)
    Found correlation with dependent variable: (59, 174)
    Found correlation with dependent variable: (60, 174)
    Found correlation with dependent variable: (61, 174)
    Found correlation with dependent variable: (62, 174)
    Found correlation with dependent variable: (63, 174)
    Found correlation with dependent variable: (64, 174)
    Found correlation with dependent variable: (65, 174)
    Found correlation with dependent variable: (66, 174)
    Found correlation with dependent variable: (67, 174)
    Found correlation with dependent variable: (68, 174)
    Found correlation with dependent variable: (69, 174)
    Found correlation with dependent variable: (70, 174)
    Found correlation with dependent variable: (71, 174)
    Found correlation with dependent variable: (72, 174)
    Found correlation with dependent variable: (73, 174)
    Found correlation with dependent variable: (74, 174)
    Found correlation with dependent variable: (75, 174)
    Found correlation with dependent variable: (76, 174)
    Found correlation with dependent variable: (77, 174)
    Found correlation with dependent variable: (78, 174)
    Found correlation with dependent variable: (79, 174)
    Found correlation with dependent variable: (80, 174)
    Found correlation with dependent variable: (81, 174)
    Found correlation with dependent variable: (82, 174)
    Found correlation with dependent variable: (83, 174)
    Found correlation with dependent variable: (84, 174)
    Found correlation with dependent variable: (85, 174)
    Found correlation with dependent variable: (86, 174)
    Found correlation with dependent variable: (87, 174)
    Found correlation with dependent variable: (88, 174)
    Found correlation with dependent variable: (89, 174)
    Found correlation with dependent variable: (90, 174)
    Found correlation with dependent variable: (91, 174)
    Found correlation with dependent variable: (92, 174)
    Found correlation with dependent variable: (93, 174)
    Found correlation with dependent variable: (94, 174)
    Found correlation with dependent variable: (95, 174)
    Found correlation with dependent variable: (96, 174)
    Found correlation with dependent variable: (97, 174)
    Found correlation with dependent variable: (98, 174)
    Found correlation with dependent variable: (99, 174)
    Found correlation with dependent variable: (100, 174)
    Found correlation with dependent variable: (101, 174)
    Found correlation with dependent variable: (102, 174)
    Found correlation with dependent variable: (103, 174)
    Found correlation with dependent variable: (104, 174)
    Found correlation with dependent variable: (105, 174)
    Found correlation with dependent variable: (106, 174)
    Found correlation with dependent variable: (107, 174)
    Found correlation with dependent variable: (108, 174)
    Found correlation with dependent variable: (109, 174)
    Found correlation with dependent variable: (110, 174)
    Found correlation with dependent variable: (111, 174)
    Found correlation with dependent variable: (112, 174)
    Found correlation with dependent variable: (113, 174)
    Found correlation with dependent variable: (114, 174)
    Found correlation with dependent variable: (115, 174)
    Found correlation with dependent variable: (116, 174)
    Found correlation with dependent variable: (117, 174)
    Found correlation with dependent variable: (118, 174)
    Found correlation with dependent variable: (119, 174)
    Found correlation with dependent variable: (120, 174)
    Found correlation with dependent variable: (121, 174)
    Found correlation with dependent variable: (122, 174)
    Found correlation with dependent variable: (123, 174)
    Found correlation with dependent variable: (124, 174)
    Found correlation with dependent variable: (125, 174)
    Found correlation with dependent variable: (126, 174)
    Found correlation with dependent variable: (127, 174)
    Found correlation with dependent variable: (128, 174)
    Found correlation with dependent variable: (129, 174)
    Found correlation with dependent variable: (130, 174)
    Found correlation with dependent variable: (131, 174)
    Found correlation with dependent variable: (132, 174)
    Found correlation with dependent variable: (133, 174)
    Found correlation with dependent variable: (134, 174)
    Found correlation with dependent variable: (135, 174)
    Found correlation with dependent variable: (136, 174)
    Found correlation with dependent variable: (137, 174)
    Found correlation with dependent variable: (138, 174)
    Found correlation with dependent variable: (139, 174)
    Found correlation with dependent variable: (140, 174)
    Found correlation with dependent variable: (141, 174)
    Found correlation with dependent variable: (142, 174)
    Found correlation with dependent variable: (143, 174)
    Found correlation with dependent variable: (144, 174)
    Found correlation with dependent variable: (145, 174)
    Found correlation with dependent variable: (146, 174)
    Found correlation with dependent variable: (147, 174)
    Found correlation with dependent variable: (148, 174)
    Found correlation with dependent variable: (149, 174)
    Found correlation with dependent variable: (150, 174)
    Found correlation with dependent variable: (151, 174)
    Found correlation with dependent variable: (152, 174)
    Found correlation with dependent variable: (153, 174)
    Found correlation with dependent variable: (154, 174)
    Found correlation with dependent variable: (155, 174)
    Found correlation with dependent variable: (156, 174)
    Found correlation with dependent variable: (157, 174)
    Found correlation with dependent variable: (158, 174)
    Found correlation with dependent variable: (159, 174)
    Found correlation with dependent variable: (160, 174)
    Found correlation with dependent variable: (161, 174)
    Found correlation with dependent variable: (162, 174)
    Found correlation with dependent variable: (163, 174)
    Found correlation with dependent variable: (164, 174)
    Found correlation with dependent variable: (165, 174)
    Found correlation with dependent variable: (166, 174)
    Found correlation with dependent variable: (167, 174)
    Found correlation with dependent variable: (168, 174)
    Found correlation with dependent variable: (169, 174)
    Found correlation with dependent variable: (170, 174)
    Found correlation with dependent variable: (171, 174)
    Found correlation with dependent variable: (172, 174)
    Found correlation with dependent variable: (173, 174)
    Elapsed time: 0.055643813sec
    Found 15051 components of significantly correlated features from a total of 15225 pairs of correlated features
    Found the following 1 components of significantly correlated features
    {0 -> Set(69, 138, 101, 0, 88, 170, 115, 5, 120, 10, 56, 142, 153, 174, 42, 24, 37, 25, 52, 14, 110, 125, 157, 20, 46, 93, 152, 57, 78, 29, 164, 106, 121, 84, 147, 61, 132, 89, 133, 116, 1, 74, 6, 60, 117, 85, 102, 28, 38, 160, 70, 21, 137, 165, 33, 92, 65, 97, 156, 9, 53, 169, 141, 109, 124, 77, 96, 173, 13, 129, 41, 134, 73, 128, 105, 2, 166, 32, 34, 148, 45, 161, 64, 17, 149, 22, 44, 59, 118, 27, 71, 12, 54, 144, 49, 86, 159, 172, 113, 81, 76, 7, 39, 98, 103, 140, 91, 66, 155, 108, 130, 135, 3, 80, 167, 35, 162, 112, 123, 145, 48, 63, 18, 150, 95, 50, 67, 16, 127, 31, 154, 11, 72, 143, 43, 99, 87, 104, 40, 26, 158, 55, 114, 171, 139, 23, 8, 75, 119, 58, 82, 151, 36, 168, 146, 30, 51, 19, 107, 4, 126, 136, 79, 94, 131, 47, 15, 163, 68, 62, 90, 111, 122, 83, 100)}
    Found a total of 175 correlated features
     */
    println( "BOSCH 3: Spearman" )
    //report_correlation(Math.spearman, Math.abs(_) >= 0.75)( chk_if_dep )(dts)
    println( "BOSCH 4: Kendall" )
    //  internal review ID : 9047913
    // http://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8176074
    // Workaround in this case to use "-XX:+UseCountedLoopSafepoints"
    // http://www.oracle.com/technetwork/java/javase/tsg-vm-149989.pdf
    // jdb -connect sun.jvm.hotspot.jdi.SACoreAttachingConnector:javaExecutable=$JAVA_HOME/bin/java,core=./core
    // jsadebugd $JAVA_HOME/bin/java ./core
    //report_correlation( Math.kendall, Math.abs( _ ) >= 0.75 )( chk_if_dep )( dts )

    // TODO: for each group check if the distribution is a multivariate normal distribution

    // TODO: create a Smile utilities object (implicit?)

    // TODO: ran correlation - Spearman and Kendall
    // TODO: other correlates in https://en.wikipedia.org/wiki/Correlation_and_dependence
    // TODO:  	JensenShannonDivergence
    // TODO: KullbackLeiblerDivergence

    // TODO: check co-linearity
    // See https://github.com/haifengl/smile/issues/86
    // ND4j, ND4s
    /*
  ltfrDesign <- matrix(0, nrow=6, ncol=6)
ltfrDesign[,1] <- c(1, 1, 1, 1, 1, 1)
ltfrDesign[,2] <- c(1, 1, 1, 0, 0, 0)
ltfrDesign[,3] <- c(0, 0, 0, 1, 1, 1)
ltfrDesign[,4] <- c(1, 0, 0, 1, 0, 0)
ltfrDesign[,5] <- c(0, 1, 0, 0, 1, 0)
ltfrDesign[,6] <- c(0, 0, 1, 0, 0, 1)
 */
    val dt = Table.create( "test2" )
    val c1 = createFloatColumn( "col1", List( 1, 1, 1, 1, 1, 1 ) )
    val c2 = createFloatColumn( "col2", List( 1, 1, 1, 0, 0, 0 ) )
    val c3 = createFloatColumn( "col3", List( 0, 0, 0, 1, 1, 1 ) )
    val c4 = createFloatColumn( "col4", List( 1, 0, 0, 1, 0, 0 ) )
    val c5 = createFloatColumn( "col5", List( 0, 1, 0, 0, 1, 0 ) )
    val c6 = createFloatColumn( "col6", List( 0, 0, 1, 0, 0, 1 ) )

    addColumns( dt, c1, c2, c3 )

    import smile.math.matrix.QRDecomposition
    /*
## $linearCombos
## $linearCombos[[1]]
## [1] 3 1 2
## 
## $linearCombos[[2]]
## [1] 6 1 4 5
## 
## 
## $remove
## [1] 3 6
     */

    import com.github.lwhite1.tablesaw.util.DoubleArrays;

    def findLinearCombo( cols: List[ FloatColumn ], c: FloatColumn ) = {
      // Create a matrix
      val am = DoubleArrays.to2dArray( cols: _* )
      // Prepare QR decomposition
      val qr = new QRDecomposition( am )
      // Create result vector
      //val r = Array.emptyFloatArray(cols.size-1)
      val rx = new Array[ Double ]( cols.size )
      // See if c is a linear combination of cols
      qr.solve( c.toDoubleArray(), rx )
      println( s"is singular : ${qr.isSingular}" )
      rx
    }

    import org.apache.commons.math3.linear.RRQRDecomposition

    // https://stat.ethz.ch/R-manual/R-devel/library/base/html/qr.html
    // functionBody(qr.coef)
    def coef( qr: RRQRDecomposition ) = {
      // x <- qr.solve(h9, y, tol = 1e-10) # or equivalently :
      // x <- qr.coef(qrh9, y) #-- is == but much better than
      //                #-- solve(h9) %*% y
      // h9 %*% x              # = y
    }

    def which( l: Array[ Double ], f: ( Double ) => Boolean ) = {
      val t = l.zipWithIndex
      t.filter ( x => f( x._1 ) ).map( _._2 )
    }
    
    def zap() = {
      
    }

    def exp1( cols: List[ FloatColumn ], dropThreshold: Double = 1e-6 ) = {
      import scala.language.postfixOps
      import org.apache.commons.math3.linear.Array2DRowRealMatrix
      import org.apache.commons.math3.linear.DefaultRealMatrixChangingVisitor

      // Create a matrix
      val am = DoubleArrays.to2dArray( cols: _* )
      println( am.map( _.mkString( "|", ",", "|" ) ).mkString( "\n" ) )
      // Prepare QR decomposition
      val m = new Array2DRowRealMatrix( am, false ) // use array, don't copy 
      val qr = new RRQRDecomposition( m )

      // R <- qr.R(qrObj)                     # extract R matrix
      val r = qr.getR
      // numColumns <- dim(R)[2]              # number of columns in R
      val numColumns = r.getColumnDimension
      // rank <- qrObj$rank                   # number of independent columns
      val rank = qr.getRank( dropThreshold / 1000 )
      println( s"Rank = $rank" )
      // pivot <- qrObj$pivot                 # get the pivot vector
      val pivotm = qr.getP

      // if (is.null(numColumns) || rank == numColumns)
      if ( ( numColumns == 0 ) || ( rank == numColumns ) ) {
        //list()                            # there are no linear combinations
        // there are no linear combinations
        println( s"Empty: rank = $rank" )
        List()
      } else {
        // p1 <- 1:rank
        val p1 = 0 to ( rank - 1 ) toArray
        // X <- R[p1, p1]                    # extract the independent columns
        // extract the independent columns
        val x = r.getSubMatrix( p1, p1 )
        println( s"x : ${( x.getRowDimension, x.getColumnDimension )}" )
        println("R:\n" + r.toString())
        println("X:\n" + x.toString())
        // Y <- R[p1, -p1, drop = FALSE]     # extract the dependent columns
        val p2 = rank to ( numColumns - 1 ) toArray
        val s1 = p1.mkString( "," )
        val s2 = p2.mkString( "," )
        println( s1 )
        println( s2 )
        val y = r.getSubMatrix( p1, p2 )
        println( s"y : ${( y.getRowDimension, y.getColumnDimension )}" )
        println( s"y : ${y.toString()}" )
        // b <- qr(X)                        # factor the independent columns
        val b = new RRQRDecomposition( x )
        // b <- qr.coef(b, Y)                # get regression coefficients of the dependent columns
        val s = b.getSolver
        val bc = s.solve( y )
        // b[abs(b) < 1e-6] <- 0             # zap small values
        println( s"coef' = ${bc.toString()}" )
        bc.walkInColumnOrder( new DefaultRealMatrixChangingVisitor {
          //override def visit(row : Int, column : Int, value: Double) = { if (Math.abs(value) < dropThreshold) 0.0 else value } 
          override def visit( row: Int, column: Int, value: Double ) = { if ( Math.abs( value ) < dropThreshold ) 0.0 else value }
        } )

        println( s"pivotM = ${pivotm.toString()}" )
        println( s"coef = ${bc.toString()}" )
        println( s"coef(row, col) = (${bc.getRowDimension},${bc.getColumnDimension})")
        println(bc.getRow(0).mkString(","))
        println(bc.getRow(1).mkString(","))
        println(bc.getColumn(0).mkString(",")) // ????
        // println(bc.getColumn(1).mkString(","))

        // Get the pivoted column inedexes
        val pivot = ( 0 to pivotm.getColumnDimension - 1 ) map { x => pivotm.getColumn( x ).indexWhere { x => x == 1.0 } }
        println( s"pivot = ${pivot.mkString( "," )}" )

        // # generate a list with one element for each dependent column
        //lapply(1:dim(Y)[2], function(i) c(pivot[rank + i], pivot[which(b[,i] != 0)]))
        val nColsy = y.getColumnDimension
        val depCols = 0 to nColsy - 1

        val l = depCols.map { x =>
          val v1 = bc.getColumn( x )
          println(s"b[,$x] = " + v1.mkString(","));
          val v2 = which( v1, { x => x != 0.0 } )
          val v3 = v2.map { x => pivot( x ) }
          println(s"rank + x = ${rank + x}")
          val dep = pivot( rank + x )
          println( s"dependent col = ${dep}" )
          println( s"dependent cols = ${v3.mkString( "," )}" )
          
          // Checking
          val ny = m.getColumnMatrix( dep )
          val rs = 0 to (m.getRowDimension -1 ) toArray
          val nm = m.getSubMatrix(rs, v3.toArray)
          val rslt = nm.multiply(bc)
          println(s"Check: ${rslt.toString()}")
          
          ( dep, v2 )
        }
        l
      }

    }

    // Ok 2 = c3 - 0, 1
    exp1( List(c1, c2, c3) )
    //Ok 3 = c6 - 0,1,2
    //exp1( List( c1, c4, c5, c6 ) )
    // Ok - selects 4 = c5 - 0,1,2,3,5
    //exp1( List(c1, c2, c3, c4, c5, c6) )
    // Empty ?
    //exp1( List(c1, c2, c3, c4, c6) )

    /*
    // http://finmath.net/
    // https://issues.apache.org/jira/browse/MATH-1101
    // https://issues.apache.org/jira/browse/MATH-1100
    def bug() = {
      import org.apache.commons.math3.linear.Array2DRowRealMatrix
      import org.apache.commons.math3.linear.DefaultRealMatrixChangingVisitor
      
      val am = new Array[Array[Double]](5)
      val c1 = Array(1.0, 1, 1, 1, 1, 1) 
      val c2 = Array(1.0, 1, 1, 0, 0, 0)
      val c3 = Array(0.0, 0, 0, 1, 1, 1)
      val c4 = Array(1.0, 0, 0, 1, 0, 0)
      //val c5 = Array(0.0, 1, 0, 0, 1, 0)
      val c6 = Array(0.0, 0, 1, 0, 0, 1)      
      
      am(0) = c1
      am(1) = c2
      am(2) = c3
      am(3) = c4
      am(4) = c6
      //am(5) = c6
      
      val m = new Array2DRowRealMatrix( am, false ) // use array, don't copy 
      val qr = new RRQRDecomposition( m, 1e-1 )
      //import org.apache.commons.math3.linear.QRDecomposition
      //val qr = new QRDecomposition( m )
      val r = qr.getR
      val numColumns = r.getColumnDimension
      val rank = qr.getRank( 1e-1 )
      println(s"QR rank: $rank")
      println(s"QR is singular: ${!qr.getSolver.isNonSingular}")
      
      import org.apache.commons.math3.linear.SingularValueDecomposition
      val sv2 = new org.apache.commons.math3.linear.SingularValueDecomposition(m);
		  //assertEquals(379, sv2.getRank());//this is OK      
      println(s"SVD rank: ${sv2.getRank()}")
      
      println(r.toString)
      //println(qr.getQ.toString)
      //println(qr.getQT.toString)
      
    }
    
    bug
    */
    /*
    def mm( cols: List[ FloatColumn ], c: Array[ Double ] ) = {
      import smile.math.Math._

      val a = DoubleArrays.to2dArray( cols: _* )
      val b = Array.ofDim[ Double ]( 1, c.size )
      b( 0 ) = c
      val r = abtmm( a, b )
      r
    }

    // We have a solution = result vector not zero 
    val cols1 = List( c2, c3 )
    val r1 = findLinearCombo( cols1, c1 )
    println( r1.mkString( "<", ",", ">" ) )
    val rr = mm( cols1, r1 )
    println( rr.map( _.mkString( "|", ",", "|" ) ).mkString( "\n" ) )
    
    // We have no solution = result is trivial with vector zero
    val cols2 = List( c1, c4 )
    val r2 = findLinearCombo( cols2, c3 )
    println( r2.mkString( "<", ",", ">" ) )
    val rr2 = mm( cols2, r2 )
    println( rr2.map( _.mkString( "|", ",", "|" ) ).mkString( "\n" ) )

    val cols3 = List( c4, c2 )
    val r3 = findLinearCombo( cols2, c3 )
    println( r3.mkString( "<", ",", ">" ) )
*/

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
