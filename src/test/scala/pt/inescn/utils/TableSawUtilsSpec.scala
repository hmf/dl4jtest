package pt.inescn.utils

import org.scalatest._

import collection.mutable.Stack
import org.scalatest._

import pt.inescn.utils.TestUtils._

/**
 *
 *  test:compile
 *  test:console
 *  test:consoleQuick
 *  test:run
 *  test:runMain
 *
 * sbt test
 * sbt testOnly "pt.inescn.utils.TableSawUtilsSpec"
 */
class TableSawUtilsSpec extends FlatSpec with Matchers {

  "check for simple near zero" should "generate near zero check table" in {
    import pt.inescn.utils.TableSawUtils._
    import pt.inescn.utils.Utils._
    import com.github.lwhite1.tablesaw.api.Table

    def assertNearZeroCheck( nzc: NearZeroCheck,
                             freqRatio: Double, badFreqRatio: Boolean,
                             uniqueRatio: Double, badUniqueRatio: Boolean,
                             isConstant: Boolean ) = {
      chk( nzc.frequency_ratio, freqRatio )
      nzc.bad_freq_ratio should be ( badFreqRatio )
      chk( nzc.unique_val_ratio, uniqueRatio )
      nzc.unique_val_ratio should be ( ( uniqueRatio ) +- precision )
      nzc.bad_unique_val_ratio should be ( badUniqueRatio )
      nzc.isConstant should be ( isConstant )
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
    assertNearZeroCheck( nzc1, 1.5, false, 0.571429, false, false )

    val nzc2 = nearZero( dt, "col2" )
    println( nzc2 )
    assertNearZeroCheck( nzc2, 1.0, false, 1.0, false, false )

    val nzc3 = nearZero( dt, "col3" )
    println( nzc3 )
    //assertNearZeroCheck( nzc3, Double.NaN, false, 0.14285714285714285, false, true )
    assertNearZeroCheck( nzc3, Double.NaN, false, 0.142857, false, true )

    val nzt1 = nearZeros( dt )
    println( nzt1.print )

  }

  "check for near zero in mdrrdesc data" should "generate same results as the Caret R package" in {

    import collection.JavaConverters._

    import pt.inescn.utils.Utils._
    import pt.inescn.utils.TableSawUtils._
    import com.github.lwhite1.tablesaw.api.Table
    import com.github.lwhite1.tablesaw.io.csv.CsvReader

    // Lest use https://topepo.github.io/caret/pre-processing.html t check the near zero calculations
    // It uses the mdrrdesc data as an example - we will use that to check the results

    // When reading the file, type inference fails. Sampling fails. Sampling of 1st row always occurs 
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
    z.foreach{ case ( a, b ) => a should be ((b) +- precision)  }


    val freqRatiosC1 = filtered.floatColumn( "freqRatio" )
    val freqRatios1 = freqRatiosC1.asScala.map( _.toDouble ).zip( List( 23.0, 131.0, 527.0, 527.0, 527.0, 21.782608, 57.666668, 527.0, 123.5, 527.0 ) )
    freqRatios1.foreach{ case ( a, b ) => a should be ((b) +- precision)  }

    val uniqueValRatioC1 = filtered.floatColumn( "uniqueValRatio" )
    val l2 = List( 0.3787879, 0.3787879, 0.3787879, 0.3787879, 0.3787879, 0.5681818, 0.3787879, 0.3787879, 5.8712121, 0.3787879 ).map( _ / 100.0 )
    val uniqueValRatio1 = uniqueValRatioC1.asScala.map( _.toDouble ).zip( l2 )
    uniqueValRatio1.foreach{ case ( a, b ) => a should be ((b) +- precision)  }

    val nzv1 = filtered.booleanColumn( "badFreqRatio" ).toIntArray
      .zip( filtered.booleanColumn( "badUniqueValRatio" ).toIntArray )
      .map( x => ( x._1 > 0 ) || ( x._2 > 0 ) )
      .toIterable
    nzv1.toSeq  should contain theSameElementsInOrderAs List( true, true, true, true, true, true, true, true, true, true )

    val const1 = filtered.booleanColumn( "isConstant" ).toIntArray.map( _ > 0 ).toIterable
    const1.toSeq  should contain theSameElementsInOrderAs List( false, false, false, false, false, false, false, false, false, false )
    
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
    isNonNumeric( ctp1 ) should be ( false )
    isNonNumeric( ctp2 ) should be ( false )
    val corr1 = applyColumns( Math.cor )( dt1, cti1, ctp1, cti2, ctp2 )
    println( s"corr = ${corr1}" )
    corr1 should be ((1.0) +- precision)

    // check all pairs of columns for correlation

    val t1 = time { combColumns( Math.cor )( dt1 ) }
    //println( t1.mkString( "<<", ",", ">>" ) )
    println( s"Calculated ${t1.size} pairs o correlations" )
    val t1cc = dt1.columnCount()
    //println(t1cc * (t1cc -1) / 2)
    t1cc * ( t1cc - 1 ) / 2 should be (t1.size)

    val t2 = time { findCorrelation( Math.cor, _ >= 0.75 )( dt1 ) }
    // println( t2.mkString( "<<", ",", ">>" ) )
    println( s"Found ${t2.size} pairs o significant correlations" )
    t1.size should be >= (t2.size)

  }

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[ Int ]
    stack.push( 1 )
    stack.push( 2 )
    stack.pop() should be ( 2 )
    stack.pop() should be ( 1 )
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[ Int ]
    a[ NoSuchElementException ] should be thrownBy {
      emptyStack.pop()
    }
  }
}